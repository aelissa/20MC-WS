options(java.parameters = "-Xmx56G")  # adjust to available RAM; 8GB recommended

library(r5r)
library(nngeo)
library(sf)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# ---------------------------------------------------------------------------
# 0. Configuration
# ---------------------------------------------------------------------------

# Path to directory containing the OSM .pbf file for Scotland
# Download from: https://download.geofabrik.de/europe/great-britain/scotland.html
R5R_DATA_DIR <- "routing/"   

# Walking time threshold in minutes — destinations reachable within this
# time will be counted. 15 minutes is the Scottish Local Living standard.
WALK_THRESHOLD_MINS <- 15

# Maximum straight-line search radius for nearest neighbours (metres).
# 2000m at ~5 km/h walking = ~24 min upper bound, giving r5r enough
# candidates to find reachable destinations without routing to distant ones.
MAX_NN_DIST_M <- 2000


# Output directory
OUT_DIR <- "routing_output/"


# Helper
prep_sf_for_r5r <- function(sf_obj) {
  coords <- st_coordinates(st_transform(sf_obj, 4326))
  sf_obj %>%
    st_drop_geometry() %>%
    mutate(
      lon = coords[, 1],
      lat = coords[, 2]
    ) %>%
    select(id, lon, lat)
}

r5r_network <- build_network(data_path = R5R_DATA_DIR, verbose = FALSE)

# ---------------------------------------------------------------------------
# 1. Load inputs
# ---------------------------------------------------------------------------

origins <- st_read("routing_input/origins.gpkg")%>%
 st_transform(4326) %>%
  mutate(
    id  = as.character(code),
    lon = st_coordinates(geom)[, 1],
    lat = st_coordinates(geom)[, 2]
  ) %>%
  select(id, lon, lat)

destinations<-st_read("routing_input/destinations.gpkg")%>%
  st_transform(4326) %>%
  mutate(id = as.character(fsq_place_id)) 

# Extract unique themes to iterate over
themes <- destinations %>%
  st_drop_geometry() %>%
  distinct(framework_category, framework_theme) %>%
  arrange(framework_category, framework_theme)

cat("Framework themes to process:", nrow(themes), "\n")
print(themes)

# ---------------------------------------------------------------------------
# 4. Main loop: one iteration per framework theme
# ---------------------------------------------------------------------------

results_list <- vector("list", nrow(themes))

for (i in seq(1,7)) {
  
  theme_cat   <- themes$framework_category[i]
  theme_name  <- themes$framework_theme[i]
  theme_label <- paste0(theme_cat, " — ", theme_name)
  
  cat("\n[", i, "/", nrow(themes), "] Processing:", theme_label, "\n")
  
  # ---- 4a. Subset destinations to this theme ----------------------------
  dest_theme <- destinations %>%
    filter(framework_category == theme_cat,
           framework_theme    == theme_name)
  
  n_dest <- nrow(dest_theme)
  cat("  Destinations in theme:", n_dest, "\n")
  
  if (n_dest == 0) {
    cat("  No destinations — skipping.\n")
    next
  }
  
  cat("  Running r5r travel_time_matrix...\n")
  
  tt_matrix <- travel_time_matrix(
    r5r_network       = r5r_network,
    origins           = origins,
    destinations      = dest_theme,
    mode              = "WALK",
    max_walk_time     = WALK_THRESHOLD_MINS,   # r5r won't route beyond this
    max_trip_duration = WALK_THRESHOLD_MINS,
    verbose           = FALSE
  )
  
  # r5r returns NA travel_time for unreachable pairs; drop these
  tt_matrix <- tt_matrix %>%
    filter(!is.na(travel_time_p50)) %>%
    rename(
      origin_id     = from_id,
      dest_id       = to_id,
      travel_time   = travel_time_p50  # r5r returns percentiles; p50 = median
    )
  
  # ----  Compute accessibility metric: count of destinations ----------
  # reachable within WALK_THRESHOLD_MINS for each origin
  
  walk_thresholds <- seq(5, 15, by = 1)
  
  theme_accessibility <- tt_matrix %>%
    group_by(origin_id) %>%
    summarise(
      min_travel_time  = min(travel_time),
      mean_travel_time = mean(travel_time),
      .groups = "drop"
    ) %>%
    bind_cols(
      map_dfc(
        setNames(walk_thresholds, paste0("n_reachable_", walk_thresholds, "min")),
        ~ tt_matrix %>%
          group_by(origin_id) %>%
          summarise(n = sum(travel_time <= .x), .groups = "drop") %>%
          pull(n)
      )
    )
  # Ensure all origins appear in output (zeros for those with no reachable destinations)
  theme_accessibility <- origins %>%
    select(id) %>%
    rename(origin_id = id) %>%
    left_join(theme_accessibility, by = "origin_id")  %>%
    mutate(
      across(starts_with("n_reachable_"), ~ replace_na(.x, 0L)),
      min_travel_time  = replace_na(min_travel_time,  NA_real_),
      mean_travel_time = replace_na(mean_travel_time, NA_real_),
      framework_category = theme_cat,
      framework_theme    = theme_name
    )
  
  results_list[[i]] <- theme_accessibility
  
  # Save intermediate result in case of interruption
  write_csv(
    theme_accessibility,
    file.path(OUT_DIR, paste0("accessibility_", gsub("[^a-zA-Z0-9]", "_", theme_label), ".csv"))
  )
}

# ---------------------------------------------------------------------------
# 5. Stop r5r (releases Java resources)
# ---------------------------------------------------------------------------
stop_r5r(r5r_network)

# ---------------------------------------------------------------------------
# 6. Combine all theme results into a single table and save
# ---------------------------------------------------------------------------

results_long <- bind_rows(results_list) %>%
  filter(!is.na(framework_theme))

# Long format (all themes, all metrics)
write_csv(results_long, file.path(OUT_DIR, "accessibility_all_themes_long.csv"))






























