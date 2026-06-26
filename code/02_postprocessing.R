library(tidyverse)
library(sf)

origins <- st_read("data/origins_bnd.gpkg") %>% st_make_valid()
classes <- st_read("data/urban-rural.gpkg")
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

# Get unique themes
themes <- unique(results_long$framework_theme)

# Process each theme separately and store results
results_by_theme <- list()

for (theme in themes) {
  
  cat("Processing theme:", theme, "\n")
  
  theme_data <- results_long %>%
    st_drop_geometry() %>%
    filter(framework_theme == theme, UR3Class != 3) %>%

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
  
  # Optional: inspect standardised values per theme
  cat("  Rows:", nrow(theme_data), "\n")
  cat("  Mean of first n_reachable col (should be ~0):", 
      round(mean(theme_data[[grep("n_reachable_", names(theme_data))[1]]], na.rm = TRUE), 4), "\n\n")
  
  results_by_theme[[theme]] <- theme_data
}

# Combine back into one dataframe 
results_standardised <- bind_rows(results_by_theme)

write.csv(results_standardised, "output/results_standardised.csv", row.names = F)

