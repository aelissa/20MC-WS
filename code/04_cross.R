library(tidyverse)
library(sf)


default_walk_score <- read_csv("output/research_data/default_walk_score.csv",
                                show_col_types = FALSE)
walking_speed_accessibility_profiles <- read_csv("output/research_data/walking-speed_accessibility_profiles.csv",
                                 show_col_types = FALSE)
origins <- st_read("ancillary_data/origins_bnd.gpkg", quiet = TRUE)

# Inner join restricts to settlement origins (UR3Class 1) — the only ones
# that were shape-clustered; default_walk_score.csv also covers accessible-
# and remote-rural origins, which have no cluster to cross against.
wact <- walking_speed_accessibility_profiles %>%
  inner_join(default_walk_score, by = "origin_id")

# ---------------------------------------------------------------------------
# Tertile: tertiles of the 10-min default score across settlement origins
# ---------------------------------------------------------------------------
tert <- quantile(wact$default_score, probs = c(1/3, 2/3), na.rm = TRUE)

wact <- wact %>%
  mutate(
    tertile = case_when(
      default_score <= tert[1] ~ "Low",
      default_score <= tert[2] ~ "Mid",
      TRUE                    ~ "High"
    ),
    tertile = factor(tertile, levels = c("Low", "Mid", "High")),
    cluster       = factor(cluster),
    typology      = paste0("C", as.character(cluster),
                           " (", cluster_label, ") · ", as.character(tertile))
  )

cat("Tertile cutpoints (default_score):\n"); print(tert)
cat("\nScore range per tertile:\n")
print(wact %>%
        group_by(tertile) %>%
        summarise(min = min(default_score, na.rm = TRUE),
                  max = max(default_score, na.rm = TRUE),
                  n   = dplyr::n(),
                  .groups = "drop"))
cat("\nTertile sizes:\n"); print(table(wact$tertile))

# ---------------------------------------------------------------------------
# Cross-tab summary
# ---------------------------------------------------------------------------
crosstab <- wact %>%
  group_by(cluster, cluster_label, tertile) %>%
  summarise(
    n                     = dplyr::n(),
    median_default_score = median(default_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cluster, tertile)

cat("\nCross-tabulation:\n"); print(crosstab)
write_csv(crosstab, "output/typology_crosstab.csv")

# Heatmap of cell counts

cap <- quantile(crosstab$n, 0.95)

p_heat <- crosstab %>%
  mutate(
    cluster_label = factor(cluster_label, levels = unique(cluster_label[order(cluster)])),
    # White labels on the dark half of the fill scale, black on the light
    # half, so counts stay legible regardless of cell colour.
    label_color = if_else(n > cap / 2, "white", "black")
  ) %>%
  ggplot(aes(x = tertile, y = cluster_label, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n, color = label_color), size = 4, show.legend = FALSE) +
  scale_color_identity() +
  scale_fill_viridis_c(option = "C", direction = -1,
                       limits = c(0, cap),
                       oob    = scales::squish) +
  theme_minimal() +
  labs(x = "Default Walk Accessibility Tertiles", y = "Walking-speed accessibility profile",
       fill = "N origins")
ggsave("output/typology_crosstab.jpeg", p_heat,
       width = 8, height = 6, dpi = 300)

#### publish research data ####

publish_wact <- wact %>%
  select(origin_id, cluster, cluster_label, UR3Class, default_score,
         tertile, typology) %>%
  arrange(origin_id)

write_csv(publish_wact, "output/research_data/wact.csv")

# ---------------------------------------------------------------------------
# Save combined-typology gpkg (for mapping / code/05_shiny_map.R and
# code/06_examples.R)
# ---------------------------------------------------------------------------
wact_sf <- origins %>%
  inner_join(wact, by = c("code" = "origin_id"))

st_write(wact_sf, "output/research_data/wact.gpkg", delete_dsn = TRUE)
cat("\nWrote output/research_data/wact.gpkg (", nrow(wact_sf), "polygons)\n")

# ---------------------------------------------------------------------------
# Data Overview figure: default accessibility tertile (the same
# national Low/Mid/High tertiles used throughout the typology, not a
# city-specific cut) for Scotland's four cities (Aberdeen, Dundee,
# Edinburgh, Glasgow), one map per city, combined into a single 2x2 figure.
# ---------------------------------------------------------------------------
library(cowplot)

origins_tertile <- origins %>%
  select(code, council) %>%
  inner_join(wact %>% select(origin_id, tertile),
             by = c("code" = "origin_id"))

cities <- tibble(
  council = c("S12000033", "S12000042", "S12000036", "S12000049"),
  city    = c("Aberdeen", "Dundee", "Edinburgh", "Glasgow")
)

city_data <- origins_tertile %>%
  inner_join(cities, by = "council") %>%
  mutate(city = factor(city, levels = cities$city))

make_city_map <- function(city_name) {
  d <- city_data %>% filter(city == city_name)
  ggplot(d) +
    geom_sf(aes(fill = tertile), color = NA) +
    scale_fill_manual(
      values = c(Low = "#CC6677", Mid = "#DDCC77", High = "#4477AA"),
      drop = FALSE, name = "Default walk accessibility score tertile level"
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
    labs(title = city_name) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

city_maps <- lapply(cities$city, make_city_map)

city_legend <- get_legend(
  city_maps[[1]] + theme(legend.position = "bottom",
                          legend.title = element_text(size = 16),
                          legend.text  = element_text(size = 14))
)
city_maps_no_legend <- lapply(city_maps, function(p) p + theme(legend.position = "none"))

city_grid <- plot_grid(plotlist = city_maps_no_legend, nrow = 2, ncol = 2,
                        labels = c("A", "B", "C", "D"))
city_maps_combined <- plot_grid(city_grid, city_legend, nrow = 2, rel_heights = c(1, 0.1))

ggsave("output/city_default_score_maps.jpeg", city_maps_combined,
       width = 10, height = 9, dpi = 400)
cat("City default-score maps written to output/city_default_score_maps.jpeg\n")

# ---------------------------------------------------------------------------
# Same 4 cities, but coloured by the full 15-class typology instead of the
# tertile alone. Encoding follows code/05_shiny_map.R's scheme: one
# base hue per tertile (colour-blind-safe, same hues as the tertile
# maps above), with 5 shades per hue (darkest = cluster 1, lightest =
# cluster 5) so shape and tertile are both visually decodable.
# ---------------------------------------------------------------------------
origins_typology <- origins %>%
  select(code, council) %>%
  inner_join(wact %>% select(origin_id, cluster, cluster_label, tertile, typology),
             by = c("code" = "origin_id"))

city_typology_data <- origins_typology %>%
  inner_join(cities, by = "council") %>%
  mutate(city = factor(city, levels = cities$city))

tertile_bases <- c(High = "#4477AA", Mid = "#DDCC77", Low = "#CC6677")
tertile_order <- names(tertile_bases)
n_clusters <- length(unique(city_typology_data$cluster))

shades_from_base <- function(base, n) {
  # n shades from dark->base->light, so cluster 1 = darkest and cluster n = lightest.
  rev(colorRampPalette(c("#ffffff", base, "#000000"))(n + 4)[3:(n + 2)])
}


typology_lookup <- city_typology_data %>%
  st_drop_geometry() %>%
  distinct(cluster, cluster_label, tertile, typology) %>%
  mutate(tertile = factor(tertile, levels = tertile_order)) %>%
  arrange(tertile, cluster)

typology_levels <- typology_lookup$typology
typology_display_labels <- paste(typology_lookup$cluster_label, typology_lookup$tertile, sep = " . ")

typology_palette <- unlist(lapply(tertile_order, function(m) shades_from_base(tertile_bases[[m]], n_clusters)))
names(typology_palette) <- typology_levels

city_typology_data <- city_typology_data %>%
  mutate(typology = factor(typology, levels = typology_levels))

make_city_typology_map <- function(city_name) {
  d <- city_typology_data %>% filter(city == city_name)
  ggplot(d) +
    geom_sf(aes(fill = typology), color = NA) +
    scale_fill_manual(values = typology_palette, labels = typology_display_labels,
                       drop = FALSE, name = "WACT") +
    guides(fill = guide_legend(ncol = 3, byrow = FALSE,
                                title.position = "top", title.hjust = 0.5)) +
    labs(title = city_name) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
}

city_typology_maps <- lapply(cities$city, make_city_typology_map)

city_typology_legend <- get_legend(
  city_typology_maps[[1]] + theme(legend.position = "bottom",
                                   legend.title = element_text(size = 16),
                                   legend.text  = element_text(size = 11))
)
city_typology_maps_no_legend <- lapply(city_typology_maps, function(p) p + theme(legend.position = "none"))

city_typology_grid <- plot_grid(plotlist = city_typology_maps_no_legend, nrow = 2, ncol = 2,
                                 labels = c("A", "B", "C", "D"))
city_typology_combined <- plot_grid(city_typology_grid, city_typology_legend,
                                     nrow = 2, rel_heights = c(1, 0.32))

ggsave("output/city_typology_maps.jpeg", city_typology_combined,
       width = 9, height = 12, dpi = 400)
cat("City typology maps written to output/city_typology_maps.jpeg\n")
