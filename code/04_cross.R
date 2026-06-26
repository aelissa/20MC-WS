library(tidyverse)
library(sf)

# ---------------------------------------------------------------------------
# Load shape-only clustering output
# ---------------------------------------------------------------------------
clusters <- st_read("output/urban_clusters.gpkg", quiet = TRUE)

# Labels written by 03_clustering.R after semantic remapping — never hardcode here.
cluster_labels <- read_csv("output/cluster_labels.csv", show_col_types = FALSE) %>%
  { setNames(.$label, .$cluster) }

# ---------------------------------------------------------------------------
# Magnitude bin: tertiles of the 10-min score across urban origins
# ---------------------------------------------------------------------------
tert <- quantile(clusters$default_score, probs = c(1/3, 2/3), na.rm = TRUE)

clusters <- clusters %>%
  mutate(
    magnitude_bin = case_when(
      default_score <= tert[1] ~ "Low",
      default_score <= tert[2] ~ "Mid",
      TRUE                   ~ "High"
    ),
    magnitude_bin = factor(magnitude_bin, levels = c("Low", "Mid", "High")),
    cluster       = factor(cluster),
    typology      = paste0("C", as.character(cluster),
                           " (", cluster_labels[as.character(cluster)], ") · ",
                           as.character(magnitude_bin))
  )

cat("Tertile cutpoints (default_score):\n"); print(tert)
cat("\nScore range per magnitude tier:\n")
print(clusters %>%
        st_drop_geometry() %>%
        group_by(magnitude_bin) %>%
        summarise(min = min(default_score, na.rm = TRUE),
                  max = max(default_score, na.rm = TRUE),
                  n   = dplyr::n(),
                  .groups = "drop"))
cat("\nMagnitude bin sizes:\n"); print(table(clusters$magnitude_bin))

# ---------------------------------------------------------------------------
# Cross-tab summary
# ---------------------------------------------------------------------------
crosstab <- clusters %>%
  st_drop_geometry() %>%
  group_by(cluster, magnitude_bin) %>%
  summarise(
    n                  = dplyr::n(),
    median_default_score = median(default_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(cluster, magnitude_bin)

cat("\nCross-tabulation:\n"); print(crosstab)
write_csv(crosstab, "output/typology_crosstab.csv")

# Heatmap of cell counts
# Cap the colour scale at the 95th percentile so a single high-count cell
# (the magnitude × shape combination that dominates) doesn't compress the
# rest of the gradient. Values above the cap render at the top colour.
cap <- quantile(crosstab$n, 0.95)

p_heat <- ggplot(crosstab,
                 aes(x = magnitude_bin, y = cluster, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_fill_viridis_c(option = "C", direction = -1,
                       limits = c(0, cap),
                       oob    = scales::squish) +
  theme_minimal() +
  labs(x = "Magnitude Tier", y = "Walking-speed accessibility profile",
       fill = "N origins")
ggsave("output/typology_crosstab.jpeg", p_heat,
       width = 8, height = 6, dpi = 300)

# ---------------------------------------------------------------------------
# Save combined-typology gpkg
# ---------------------------------------------------------------------------
st_write(clusters, "output/20MN_typology.gpkg", delete_dsn = TRUE)
cat("\nWrote output/20MN_typology.gpkg (", nrow(clusters), "polygons)\n")
