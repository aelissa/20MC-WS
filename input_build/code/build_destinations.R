library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(purrr)
library(httr2)

`%||%` <- function(a, b) if (is.null(a)) b else a

OUT_CRS <- 4326
RAW_DIR <- file.path("input_build", "row_input")
OUT_DIR <- file.path("input_build", "destination")
FS_PATH <- file.path(RAW_DIR, "geoDS_FS_POI.gpkg")
OSM_PATH <- file.path(RAW_DIR, "scotland.gpkg")

# Different source files name their geometry column differently ("geom" vs
# "geometry"). bind_rows() on sf objects does NOT reconcile that — it keeps
# both as separate columns, silently blanking out whichever one loses, which
# then gets dropped by any later st_intersects/filter_to_scotland() call.
# Every reader below normalises to "geometry" so sources combine safely.
set_geometry_name <- function(x, name = "geometry") {
  old <- attr(x, "sf_column")
  if (!identical(old, name)) {
    names(x)[names(x) == old] <- name
    st_geometry(x) <- name
  }
  x
}

read_fs_pois <- function(category_map) {
  cats_sql <- paste(sprintf("'%s'", gsub("'", "''", category_map$category_name)), collapse = ",")
  q <- sprintf(
    "SELECT name, category_name, geom FROM uk_retail_pois WHERE category_name IN (%s) AND lat BETWEEN %f AND %f AND lng BETWEEN %f AND %f",
    cats_sql, scot_bb["ymin"], scot_bb["ymax"], scot_bb["xmin"], scot_bb["xmax"]
  )
  st_read(FS_PATH, query = q, quiet = TRUE) %>%
    set_geometry_name() %>%
    st_transform(OUT_CRS) %>%
    left_join(category_map, by = "category_name") %>%
    transmute(name, category, source = "Foursquare (geoDS_FS_POI)")
}

# Local Geofabrik-style OSM Scotland extract. category_map: tibble(fclass, category).
# Points come from the *_free point layer; polygons from the *_a_free layer are
# reduced to centroids so every destination is a point, same as every other source.
read_osm_pois <- function(category_map, layer_pt = "gis_osm_pois_free", layer_poly = "gis_osm_pois_a_free") {
  fclass_sql <- paste(sprintf("'%s'", category_map$fclass), collapse = ",")
  q <- function(layer) sprintf("SELECT name, fclass, geom FROM %s WHERE fclass IN (%s)", layer, fclass_sql)

  pts <- st_read(OSM_PATH, query = q(layer_pt), quiet = TRUE) %>% set_geometry_name()
  polys <- st_read(OSM_PATH, query = q(layer_poly), quiet = TRUE) %>% set_geometry_name() %>% st_centroid()

  bind_rows(pts, polys) %>%
    st_transform(OUT_CRS) %>%
    left_join(category_map, by = "fclass") %>%
    transmute(name, category, source = "OpenStreetMap (local extract)")
}

out_path <- function(theme_slug) file.path(OUT_DIR, sprintf("destination_%s.gpkg", theme_slug))

make_sf <- function(df, lon = "lon", lat = "lat") {
  df %>%
    filter(!is.na(.data[[lon]]), !is.na(.data[[lat]])) %>%
    st_as_sf(coords = c(lon, lat), crs = OUT_CRS, remove = TRUE)
}

# ---------------------------------------------------------------------------
# Scotland boundary (for spatially filtering GB-wide input files). Simplified
# + buffered so legitimate coastal POIs (ferry terminals etc.) aren't
# clipped at the coastline.
# ---------------------------------------------------------------------------

scot_boundary <- st_read("ancillary_data/urban-rural.gpkg", layer = "sg_urbanrural_2022", quiet = TRUE) %>%
  st_make_valid() %>%
  st_simplify(dTolerance = 50) %>%
  st_union() %>%
  st_sf(geometry = .) %>%
  st_buffer(200)

scot_boundary_wgs84 <- st_transform(scot_boundary, OUT_CRS)
scot_bb <- st_bbox(scot_boundary_wgs84)

filter_to_scotland <- function(sf_obj, boundary = scot_boundary_wgs84) {
  sf_obj[lengths(st_intersects(st_transform(sf_obj, st_crs(boundary)), boundary)) > 0, ]
}

# ---------------------------------------------------------------------------
# Postcode geocoding (postcodes.io bulk lookup) for input files that only
# carry a postcode, not coordinates (GPs, dentists, pharmacies, hospitals,
# nurseries).
# ---------------------------------------------------------------------------

geocode_postcodes <- function(postcodes) {
  postcodes <- trimws(postcodes)
  batches <- split(postcodes, ceiling(seq_along(postcodes) / 100))
  map_dfr(batches, function(batch) {
    resp <- request("https://api.postcodes.io/postcodes") %>%
      req_method("POST") %>%
      req_body_json(list(postcodes = batch)) %>%
      req_timeout(30) %>%
      req_retry(max_tries = 3) %>%
      req_perform()
    js <- resp_body_json(resp, simplifyVector = FALSE)
    Sys.sleep(0.2)
    map_dfr(js$result, function(r) {
      tibble(
        postcode_query = r$query,
        lon = if (!is.null(r$result)) r$result$longitude else NA_real_,
        lat = if (!is.null(r$result)) r$result$latitude else NA_real_
      )
    })
  })
}

# ===========================================================================
# 1. PUBLIC TRANSPORT  — NaPTAN (input_data), GB-wide -> filter to Scotland
# ===========================================================================

build_public_transport <- function() {
  naptan <- read_csv(file.path(RAW_DIR, "NaPTAN_Stops.csv"), col_types = cols(.default = "c"))

  stop_groups <- tribble(
    ~StopType, ~category,
    "BCT", "Bus Stop", "BCS", "Bus Stop", "BCE", "Bus Stop", "BCQ", "Bus Stop", "BST", "Bus Stop",
    "RLY", "Rail Station", "RSE", "Rail Station", "RPL", "Rail Station",
    "MET", "Metro/Tram Station", "TMU", "Metro/Tram Station", "PLT", "Metro/Tram Station",
    "FER", "Ferry Terminal", "FBT", "Ferry Terminal", "FTD", "Ferry Terminal"
  )

  pts <- naptan %>%
    filter(Status == "active") %>%
    inner_join(stop_groups, by = "StopType") %>%
    transmute(
      name = CommonName,
      category,
      lon = as.numeric(Longitude),
      lat = as.numeric(Latitude)
    ) %>%
    make_sf() %>%
    filter_to_scotland() %>%
    mutate(framework_category = "Movement", framework_theme = "Public Transport", source = "NaPTAN") %>%
    select(name, category, framework_category, framework_theme, source)

  st_write(pts, out_path("public_transport"), delete_dsn = TRUE)
  pts
}

# ===========================================================================
# 2. SUPPORT AND SERVICES — Care Inspectorate, gov.scot schools,
#    NHS Scotland GPs/dentists/pharmacies, hospitals (all input_data)
# ===========================================================================

build_support_and_services <- function() {
  nurseries <- read_csv(
    file.path(RAW_DIR, "care_inspectorate.csv"),
    locale = locale(encoding = "latin1"), col_types = cols(.default = "c")
  ) %>%
    filter(CareService == "Day Care of Children", ServiceStatus == "Active") %>%
    transmute(name = ServiceName, postcode = Service_Postcode, category = "Nursery")

  schools_raw <- read_excel(file.path(RAW_DIR, "Scottish_School_PSS.xlsx"), sheet = "Schools_Final")
  schools <- schools_raw %>%
    transmute(
      name = SchoolName,
      category = paste(SchoolType, "School"),
      lon = as.numeric(Longitude),
      lat = as.numeric(Latitude)
    )

  hospitals <- read_csv(file.path(RAW_DIR, "hospitals.csv"), col_types = cols(.default = "c")) %>%
    transmute(name = HospitalName, postcode = Postcode, category = "Hospital")

  gps <- read_csv(file.path(RAW_DIR, "practice_contact_details_20261001_opendata.csv"), col_types = cols(.default = "c")) %>%
    transmute(name = GPPracticeName, postcode = Postcode, category = "GP")

  dentists <- read_csv(
    file.path(RAW_DIR, "nhs-dental-practices-and-nhs-dental-registrations-as-at-31-mar-2026.csv"),
    col_types = cols(.default = "c")
  ) %>%
    transmute(name = address1, postcode = pc7, category = "Registered Dentist")

  pharmacies <- read_csv(file.path(RAW_DIR, "dispenser_contactdetails_202607.csv"), col_types = cols(.default = "c")) %>%
    transmute(name = DispLocationName, postcode = DispLocationPostcode, category = "Pharmacy")

  needs_geocoding <- bind_rows(nurseries, hospitals, gps, dentists, pharmacies)
  geocoded <- geocode_postcodes(unique(na.omit(needs_geocoding$postcode)))
  needs_geocoding <- needs_geocoding %>%
    left_join(geocoded, by = c("postcode" = "postcode_query"), relationship = "many-to-one")

  pts <- bind_rows(needs_geocoding %>% select(name, category, lon, lat), schools) %>%
    make_sf() %>%
    filter_to_scotland() %>%
    mutate(framework_category = "Health and Wellbeing", framework_theme = "Support and Services", source = "official (see category)") %>%
    select(name, category, framework_category, framework_theme, source)

  st_write(pts, out_path("support_and_services"), delete_dsn = TRUE)
  pts
}

# ===========================================================================
# 3. NATURAL SPACE — OS Open Greenspace access points (input_data, GB-wide)
#    for Park/Garden only. No OSM: Forest, Natural Park, Botanical Garden
#    are dropped for now — no input_data source for them.
# ===========================================================================

build_natural_space <- function() {
  access_pts <- st_read(file.path(RAW_DIR, "GS_access.gpkg"), layer = "access_point", quiet = TRUE)
  sites <- st_read(file.path(RAW_DIR, "GS_access.gpkg"), layer = "greenspace_site", quiet = TRUE) %>%
    st_drop_geometry() %>%
    select(id, function_ = `function.`, distinctive_name_1)

  parks <- access_pts %>%
    left_join(sites, by = c("ref_to_greenspace_site" = "id")) %>%
    filter(function_ == "Public Park Or Garden") %>%
    st_transform(OUT_CRS) %>%
    filter_to_scotland() %>%
    transmute(name = distinctive_name_1, category = "Park/Garden", source = "OS Open Greenspace")

  pts <- parks %>%
    mutate(framework_category = "Environment", framework_theme = "Natural Space") %>%
    select(name, category, framework_category, framework_theme, source)

  st_write(pts, out_path("natural_space"), delete_dsn = TRUE)
  pts
}

# ===========================================================================
# 4. PLAY AND RECREATION — sportscotland facilities + OS Greenspace "Play
#    Space" (input_data, both Scotland-specific). No OSM: Basketball Court
#    and Stadium are dropped for now — no input_data source for them.
# ===========================================================================

build_play_and_recreation <- function() {
  sdir <- file.path(RAW_DIR, "Sports_Facilities_-_Scotland")
  read_sf_pts <- function(shp, category, filter_expr = NULL) {
    x <- st_read(file.path(sdir, shp), quiet = TRUE)
    if (!is.null(filter_expr)) x <- filter_expr(x)
    x %>% st_transform(OUT_CRS) %>% transmute(name = site_name, category = category)
  }

  sport_pts <- bind_rows(
    read_sf_pts("pub_spfat.shp", "Athletic Field"),
    read_sf_pts("pub_spfbg.shp", "Bowling Alley"),            # lawn bowls, not tenpin — closest available
    read_sf_pts("pub_spffs.shp", "Fitness Centre/Gym"),
    read_sf_pts("pub_spfsh.shp", "Fitness Centre/Gym", \(x) filter(x, fac_sub_ty == "Gymnasium")),
    read_sf_pts("pub_spfgc.shp", "Golf Course"),
    read_sf_pts("pub_spfsh.shp", "Sports Centre", \(x) filter(x, fac_sub_ty != "Gymnasium" | is.na(fac_sub_ty))),
    read_sf_pts("pub_spfsp.shp", "Swimming Pool"),
    read_sf_pts("pub_spfitc.shp", "Tennis Court"),
    read_sf_pts("pub_spfotc.shp", "Tennis Court")
  )

  access_pts <- st_read(file.path(RAW_DIR, "GS_access.gpkg"), layer = "access_point", quiet = TRUE)
  sites <- st_read(file.path(RAW_DIR, "GS_access.gpkg"), layer = "greenspace_site", quiet = TRUE) %>%
    st_drop_geometry() %>%
    select(id, function_ = `function.`, distinctive_name_1)

  playgrounds <- access_pts %>%
    left_join(sites, by = c("ref_to_greenspace_site" = "id")) %>%
    filter(function_ == "Play Space") %>%
    st_transform(OUT_CRS) %>%
    filter_to_scotland() %>%
    transmute(name = distinctive_name_1, category = "Playground")

  pts <- bind_rows(
    sport_pts %>% mutate(source = "sportscotland"),
    playgrounds %>% mutate(source = "OS Open Greenspace")
  ) %>%
    filter_to_scotland() %>%
    mutate(framework_category = "Health and Wellbeing", framework_theme = "Play and Recreation") %>%
    select(name, category, framework_category, framework_theme, source)

  st_write(pts, out_path("play_and_recreation"), delete_dsn = TRUE)
  pts
}

# ===========================================================================
# 5. CIVIC AND COMMUNITY — Historic Environment Scotland Properties in Care
#    (input_data); OSM for everything else (no official source available)
# ===========================================================================

build_civic_and_community <- function() {
  pic <- st_read(file.path(RAW_DIR, "pic", "properties_in_care.shp"), quiet = TRUE) %>%
    st_transform(OUT_CRS) %>%
    transmute(name = PIC_NAME, category = "Historic and Cultural Site", source = "Historic Environment Scotland")

  osm_map <- tribble(
    ~fclass, ~category,
    "atm", "ATM",
    "town_hall", "Town Hall",
    "public_building", "Public Building",
    "courthouse", "Courthouse",
    "library", "Library",
    "police", "Police Station",
    "post_office", "Post Office"
  )
  osm_pts <- read_osm_pois(osm_map)

  worship_pts <- bind_rows(
    st_read(OSM_PATH, layer = "gis_osm_pofw_free", quiet = TRUE) %>% set_geometry_name(),
    st_read(OSM_PATH, layer = "gis_osm_pofw_a_free", quiet = TRUE) %>% set_geometry_name() %>% st_centroid()
  ) %>%
    st_transform(OUT_CRS) %>%
    transmute(name, category = "Place of Worship", source = "OpenStreetMap (local extract)")

  pts <- bind_rows(pic, osm_pts, worship_pts) %>%
    filter_to_scotland() %>%
    mutate(framework_category = "Civic", framework_theme = "Civic and Community") %>%
    select(name, category, framework_category, framework_theme, source)

  st_write(pts, out_path("civic_and_community"), delete_dsn = TRUE)
  pts
}

# ===========================================================================
# 6. SOCIAL INTERACTION — geoDS_FS_POI (Foursquare, input_data) for most
#    categories; local OSM extract for Museum, Zoo, Cultural Centre (the
#    only FS-missing categories the extract actually covers).
# ===========================================================================

build_social_interaction <- function() {
  fs_map <- tribble(
    ~category_name, ~category,
    "Restaurant", "Restaurant",
    "Bar", "Bar",
    "Pub", "Bar",
    "Gastropub", "Bar",
    "Coffee Shop", "Café",
    "Cafe, Coffee, and Tea House", "Café",
    "Tea Room", "Café",
    "Art Gallery", "Art Gallery",
    "Night Club", "Nightclub",
    "Music Venue", "Concert Hall",
    "Concert Hall", "Concert Hall",
    "Jazz and Blues Venue", "Concert Hall",
    "Performing Arts Venue", "Theatre",
    "Theater", "Theatre",
    "Indie Theater", "Theatre",
    "Movie Theater", "Cinema",
    "Indie Movie Theater", "Cinema",
    "Drive-in Theater", "Cinema",
    "Community Center", "Community Centre",
    "Amphitheater", "Amphitheater"
  )
  fs_pts <- read_fs_pois(fs_map)

  # Aquarium, Conference Centre, Event Space, and Exhibition Centre have no
  # fclass in the local extract's POI schema — dropped, same as the other
  # no-source categories elsewhere in this script.
  osm_map <- tribble(
    ~fclass, ~category,
    "museum", "Museum",         # covers Art Museum too — the extract doesn't distinguish
    "zoo", "Zoo",
    "arts_centre", "Cultural Centre"
  )
  osm_pts <- read_osm_pois(osm_map)

  pts <- bind_rows(fs_pts, osm_pts) %>%
    filter_to_scotland() %>%
    mutate(framework_category = "Social", framework_theme = "Social Interaction") %>%
    select(name, category, framework_category, framework_theme, source)

  st_write(pts, out_path("social_interaction"), delete_dsn = TRUE)
  pts
}

# ===========================================================================
# 7. HOUSING AND COMMUNITY — geoDS_FS_POI (Foursquare, input_data) covers
#    every named category, including "Retail" (Fashion, Furniture and Home,
#    Sporting Goods, Computers and Electronics); no OSM needed here.
# ===========================================================================

build_housing_and_community <- function() {
  fs_map <- tribble(
    ~category_name, ~category,
    "Bakery", "Bakery",
    "Butcher", "Butcher",
    "Cheese Store", "Cheese Store",
    "Convenience Store", "Convenience Store",
    "Deli", "Deli",
    "Market", "Food Market",
    "Farmers Market", "Food Market",
    "Fish Market", "Food Market",
    "Fruit and Vegetable Store", "Greengrocer",
    "Supermarket", "Supermarket",
    "Grocery Store", "Supermarket",
    "Organic Grocery", "Supermarket",
    # Retail — Fashion Retail
    "Clothing Store", "Retail",
    "Jewelry Store", "Retail",
    "Shoe Store", "Retail",
    "Bridal Store", "Retail",
    "Women's Store", "Retail",
    "Men's Store", "Retail",
    "Fashion Accessories Store", "Retail",
    "Children's Clothing Store", "Retail",
    "Lingerie Store", "Retail",
    "Fashion Retail", "Retail",
    "Watch Store", "Retail",
    "Sunglasses Store", "Retail",
    # Retail — Furniture and Home Store
    "Furniture and Home Store", "Retail",
    "Housewares Store", "Retail",
    "Home Appliance Store", "Retail",
    "Carpet Store", "Retail",
    "Kitchen Supply Store", "Retail",
    "Mattress Store", "Retail",
    "Lighting Store", "Retail",
    # Retail — Sporting Goods Retail
    "Sporting Goods Retail", "Retail",
    "Bicycle Store", "Retail",
    "Fishing Store", "Retail",
    "Golf Store", "Retail",
    "Ski Store", "Retail",
    "Skate Store", "Retail",
    "Running Store", "Retail",
    "Surf Store", "Retail",
    "Hunting Supply Store", "Retail",
    "Gun Store", "Retail",
    # Retail — Computers and Electronics Retail
    "Mobile Phone Store", "Retail",
    "Electronics Store", "Retail",
    "Video Games Store", "Retail",
    "Computers and Electronics Retail", "Retail",
    "Camera Store", "Retail"
  )

  pts <- read_fs_pois(fs_map) %>%
    filter_to_scotland() %>%
    mutate(framework_category = "Living", framework_theme = "Housing and Community") %>%
    select(name, category, framework_category, framework_theme, source)

  st_write(pts, out_path("housing_and_community"), delete_dsn = TRUE)
  pts
}

# ===========================================================================
# Run
# ===========================================================================

build_public_transport()
build_support_and_services()
build_natural_space()
build_play_and_recreation()
build_civic_and_community()
build_social_interaction()
build_housing_and_community()
