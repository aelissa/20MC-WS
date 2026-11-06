library(tidyverse)
library(sf)

origins <- st_read("ancillary_data/origins_bnd.gpkg") %>% st_make_valid()
classes <- st_read("ancillary_data/urban-rural.gpkg")
results_long <- read_csv("routing_output/accessibility_all_themes_long.csv")

# Parse geom column (stored as R vector notation) and assign UR3Class via spatial intersection
parse_coords <- function(geom_str) {
  nums <- as.numeric(regmatches(geom_str, gregexpr("-?[0-9]+[.][0-9]+", geom_str))[[1]])
  st_point(nums)
}

results_long <- results_long %>%
  mutate(geometry = st_sfc(lapply(geom, parse_coords), crs = 4326)) %>%
  st_as_sf() %>%
  st_transform(st_crs(classes)) %>%
  st_join(classes %>% select(UR3Class), join = st_intersects) %>%
  select(!geom)

# ---------------------------------------------------------------------------
# Accessibility validation: origins with no route found in ANY theme, and
# average travel time to the nearest destination, by UR3 class.
# ---------------------------------------------------------------------------
dir.create("output/accessibility_validation", showWarnings = FALSE)

ur3_labels <- c("1" = "Rest of Scotland (settlements)",
                "2" = "Accessible Rural",
                "3" = "Remote Rural")

# Diagnostic: origins with no routable path to any destination in any theme,
# before excluding the invalid ones below.
all_na_diagnostic <- results_long %>%
  st_drop_geometry() %>%
  group_by(origin_id, UR3Class) %>%
  summarise(all_na = all(is.na(min_travel_time)), .groups = "drop") %>%
  filter(all_na)

# Invalid origins: no routable path to any destination in any of the 7
# framework themes despite sitting in dense urban areas (Aberdeen City,
# Glasgow City), where that outcome is implausible -- consistent with a
# routing/network-connectivity artifact (e.g. the origin point snapping to
# a pedestrian-inaccessible fragment of the street graph) rather than
# genuine isolation are excluded.

invalid_ids <- c("S00137067", "S00158294", "S00160172", "S00160189")
results_long <- results_long %>% filter(!origin_id %in% invalid_ids)

na_table <- all_na_diagnostic %>%
  mutate(flagged_invalid = origin_id %in% invalid_ids) %>%
  group_by(UR3Class) %>%
  summarise(
    n_all_na_diagnostic = dplyr::n(),
    n_flagged_invalid = sum(flagged_invalid),
    n_all_na_final = n_all_na_diagnostic - n_flagged_invalid,
    .groups = "drop"
  ) %>%
  mutate(UR3Name = ur3_labels[as.character(UR3Class)], .after = UR3Class)

cat("Origins with no route in any theme (diagnostic, before removal):",
    nrow(all_na_diagnostic), "\n")
print(na_table)
write_csv(na_table, "output/accessibility_validation/na_origins_by_ur3.csv")

# Average travel time to nearest destination per origin, summarised by UR3
# class, on the cleaned data (invalid origins already excluded above).
avg_travel_time_table <- results_long %>%
  st_drop_geometry() %>%
  group_by(origin_id, UR3Class) %>%
  summarise(avg_travel_time = mean(min_travel_time, na.rm = TRUE), .groups = "drop") %>%
  group_by(UR3Class) %>%
  summarise(
    n_origins = dplyr::n(),
    mean_avg_travel_time = round(mean(avg_travel_time, na.rm = TRUE), 2),
    median_avg_travel_time = round(median(avg_travel_time, na.rm = TRUE), 2),
    q25 = round(quantile(avg_travel_time, 0.25, na.rm = TRUE), 2),
    q75 = round(quantile(avg_travel_time, 0.75, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(UR3Name = ur3_labels[as.character(UR3Class)], .after = UR3Class)

cat("\nAverage travel time to nearest destination, by UR3 class (invalid origins excluded):\n")
print(avg_travel_time_table)
write_csv(avg_travel_time_table, "output/accessibility_validation/avg_travel_time_by_ur3.csv")

#### publish research data ####
# Routing always assumes the standard
# reference pace of 60 m/min; X rescales that pace
# proportionally (walking_speed_m_per_min = X / 10 * 60 = X * 6), so X = 5 is
# a slow walker (30 m/min) and X = 15 is a fast walker (90 m/min) to capture what 
# 10 minutes are for people walking at different speeds.

publish_accessibility <- results_long %>%
  st_drop_geometry() %>%
  pivot_longer(
    cols = starts_with("n_reachable_"),
    names_to = "walk_speed_label",
    names_pattern = "n_reachable_([0-9]+)min",
    names_transform = as.integer,
    values_to = "n_reachable"
  ) %>%
  mutate(
    walking_speed_m_per_min = walk_speed_label * 6,
    # travel_time was computed at the 60 m/min reference pace; rescale to
    # this row's own walking speed. Time is inversely proportional to speed
    # for a fixed distance, and that scaling factor is the same for every
    # destination.
    min_travel_time_at_speed  = min_travel_time  * (60 / walking_speed_m_per_min)
  ) %>%
  select(
    origin_id, framework_theme, UR3Class,
    walking_speed_m_per_min, n_reachable,
    min_travel_time_at_speed
  ) %>%
  arrange(origin_id, framework_theme, walking_speed_m_per_min)

write_csv(publish_accessibility, "output/research_data/accessibility_walkspeeds.csv")

# Get unique themes
themes <- unique(results_long$framework_theme)

# Process each theme separately and store results
results_by_theme <- list()

for (theme in themes) {
  
  cat("Processing theme:", theme, "\n")
  
  theme_data <- results_long %>%
    st_drop_geometry() %>%
    filter(framework_theme == theme) %>%

    # Standardise within theme x UR3Class
    group_by(UR3Class) %>%
    mutate(
      across(
        starts_with("n_reachable_"),
        ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)
      )
    ) %>%
    ungroup() %>%

    # Summarise by origin and class
    group_by(origin_id, framework_theme, UR3Class) %>%
    summarise(
      across(starts_with("n_reachable_"), sum),
      .groups = "drop"
    )
  
  # inspect standardised values per theme
  cat("  Rows:", nrow(theme_data), "\n")
  cat("  Mean of first n_reachable col (should be ~0):", 
      round(mean(theme_data[[grep("n_reachable_", names(theme_data))[1]]], na.rm = TRUE), 4), "\n\n")
  
  results_by_theme[[theme]] <- theme_data
}

# Combine back into one dataframe
accessibility_walkspeed_standardised <- bind_rows(results_by_theme)

#### publish research data ####

publish_accessibility_walkspeed_standardised <- accessibility_walkspeed_standardised %>%
  pivot_longer(
    cols = starts_with("n_reachable_"),
    names_to = "walk_speed_label",
    names_pattern = "n_reachable_([0-9]+)min",
    names_transform = as.integer,
    values_to = "n_reachable_standardised"
  ) %>%
  mutate(walking_speed_m_per_min = walk_speed_label * 6) %>%
  select(
    origin_id, framework_theme, UR3Class,
    walking_speed_m_per_min, n_reachable_standardised
  ) %>%
  arrange(origin_id, framework_theme, walking_speed_m_per_min)

write_csv(publish_accessibility_walkspeed_standardised, "output/research_data/accessibility_walkspeed_standardised.csv")


# ---------------------------------------------------------------------------
# Default score (sum of standardised n_reachable_10min across themes)
# ---------------------------------------------------------------------------
default_walk_score <- accessibility_walkspeed_standardised %>%
  group_by(origin_id, UR3Class) %>%
  summarise(default_score = sum(n_reachable_10min, na.rm = TRUE), .groups = "drop")

#### publish research data ####

write_csv(default_walk_score, "output/research_data/default_walk_score.csv")


