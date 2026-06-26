library(tidyverse)
library(sf)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
results_long         <- read_csv("routing_output/accessibility_all_themes_long.csv",
                                 show_col_types = FALSE)
results_standardised <- read_csv("output/results_standardised.csv", show_col_types = FALSE)
origins              <- st_read("data/origins_bnd.gpkg", quiet = TRUE)

# Urban origin_ids only
urban_ids <- results_standardised %>%
  filter(UR3Class == 1) %>%
  distinct(origin_id) %>%
  pull(origin_id)

# ---------------------------------------------------------------------------
# Build shape-only curves
# ---------------------------------------------------------------------------
n_reach_cols <- grep("^n_reachable_", names(results_long), value = TRUE)

curves <- results_long %>%
  filter(origin_id %in% urban_ids) %>%
  group_by(origin_id) %>%
  summarise(across(all_of(n_reach_cols), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop")

curves_mat  <- as.matrix(curves[, n_reach_cols])
row_max     <- apply(curves_mat, 1, max)
keep        <- row_max > 0                                 # drop origins with all zeros
curves      <- curves[keep, ]
curves_mat  <- curves_mat[keep, ]
curves_norm <- curves_mat / row_max[keep]

cat("Origins clustered:", nrow(curves_norm), "\n")

# ---------------------------------------------------------------------------
# Default score (sum of standardised n_reachable_10min across themes, urban)
# ---------------------------------------------------------------------------
urban_default <- results_standardised %>%
  filter(UR3Class == 1) %>%
  group_by(origin_id) %>%
  summarise(default_score = sum(n_reachable_10min, na.rm = TRUE), .groups = "drop")

# ---------------------------------------------------------------------------
# Clustergram (k = 2..8) — Galili's implementation in code/clustergram.R
# ---------------------------------------------------------------------------
source("code/clustergram.R")
set.seed(42)

jpeg("output/clustergram.jpeg", width = 1400, height = 900, res = 110)
par(cex.lab = 1.2, cex.main = 0.9)
clustergram(curves_norm, k.range = 2:8, line.width = 0.00001, add.center.points = TRUE)
dev.off()
cat("Clustergram written to output/clustergram.jpeg\n")

# ---------------------------------------------------------------------------
# Final clustering — change K after inspecting the clustergram
# ---------------------------------------------------------------------------
K <- 5
km <- kmeans(curves_norm, centers = K, nstart = 25, iter.max = 100)

# ---------------------------------------------------------------------------
# Renumber clusters by centroid baseline so the numbering is stable across
# re-runs. Clusters are ranked
# by their value at the slowest walking speed (first n_reach column), descending:
# C1 = highest near-home baseline, C5 = lowest. Labels follow this ordering.
# ---------------------------------------------------------------------------
baseline_rank  <- order(km$centers[, 1], decreasing = TRUE)  # highest baseline first
remap          <- integer(K)
remap[baseline_rank] <- seq_len(K)                # km cluster -> ranked number
curves$cluster <- factor(remap[km$cluster])

cat("Cluster sizes (k =", K, ", renumbered by baseline):\n")
print(table(curves$cluster))

cluster_labels <- c(
  "1" = "Near-home concentration",
  "2" = "Mid-distance concentration",
  "3" = "Gradual, well-served",
  "4" = "Mid-range linear",
  "5" = "Far-only / sparse near home"
)
write_csv(tibble(cluster = names(cluster_labels), label = unname(cluster_labels)),
          "output/cluster_labels.csv")

# Reorder centroid rows to match new semantic cluster numbers
centers_remapped <- km$centers[match(seq_len(K), remap), ]

# Centroids plot for interpretation
centroids <- as_tibble(centers_remapped) %>%
  mutate(cluster = factor(seq_len(K))) %>%
  pivot_longer(cols = all_of(n_reach_cols),
               names_to = "walking_col", values_to = "norm_value") %>%
  mutate(
    minutes       = as.integer(sub("n_reachable_(\\d+)min", "\\1", walking_col)),
    walking_speed = minutes * 6
  )

p_centroids <- ggplot(centroids, aes(x = walking_speed, y = norm_value, color = cluster)) +
  geom_line(linewidth = 1.1) +
  theme_minimal() +
  labs(title = paste("Cluster centroids (k =", K, ")"),
       x = "Walking speed (m/min)", y = "Normalised accessibility (row max = 1)")
ggsave("output/cluster_centroids.jpeg", p_centroids,
       width = 9, height = 6, dpi = 300)

write_csv(centroids %>% select(cluster, walking_speed, norm_value),
          "output/cluster_centroids.csv")

# ---------------------------------------------------------------------------
# Cluster validation — stability under re-seeding + bootstrap stability
# ---------------------------------------------------------------------------
# Requires: mclust (for Adjusted Rand Index), fpc (for clusterboot)

if (!requireNamespace("mclust", quietly = TRUE)) {
  message("Skipping re-seed stability: install package 'mclust'.")
} else {
  cat("\n--- Stability under re-seeding ---\n")
  n_seeds <- 100
  canonical <- km$cluster

  aris <- vapply(seq_len(n_seeds), function(i) {
    set.seed(1000 + i)
    km_i <- kmeans(curves_norm, centers = K, nstart = 10)
    mclust::adjustedRandIndex(canonical, km_i$cluster)
  }, numeric(1))

  cat(sprintf("ARI across %d re-seedings (nstart = 10) — median: %.3f, min: %.3f, max: %.3f\n",
              n_seeds, median(aris), min(aris), max(aris)))
  write_csv(tibble(seed = seq_len(n_seeds), ARI = aris, nstart = 10),
            "output/stability_reseed.csv")

  # Stress test: nstart = 1 (single random start per refit).
  # The first test confirms the *canonical solution* is reproducible. This one
  # confirms the *cluster structure itself* is recoverable from a single
  # random initialisation, which is a more demanding test.
  
  cat("\n--- Stress test: nstart = 1 ---\n")
  aris_n1 <- vapply(seq_len(n_seeds), function(i) {
    set.seed(2000 + i)
    km_i <- kmeans(curves_norm, centers = K, nstart = 1, iter.max = 100)
    mclust::adjustedRandIndex(canonical, km_i$cluster)
  }, numeric(1))

  cat(sprintf("ARI across %d re-seedings (nstart = 1) — median: %.3f, min: %.3f, max: %.3f\n",
              n_seeds, median(aris_n1), min(aris_n1), max(aris_n1)))
  write_csv(
    tibble(seed = seq_len(n_seeds), ARI = aris_n1, nstart = 1),
    "output/stability_reseed_nstart1.csv"
  )
}

if (!requireNamespace("fpc", quietly = TRUE)) {
  message("Skipping bootstrap stability: install package 'fpc'.")
} else {
  cat("\n--- Bootstrap stability (fpc::clusterboot) ---\n")
  set.seed(42)
  boot_res <- fpc::clusterboot(
    curves_norm,
    B             = 100,                       # bootstrap resamples
    clustermethod = fpc::kmeansCBI,
    krange        = K,
    seed          = 42,
    count         = FALSE
  )
  cat("Mean Jaccard similarity per cluster (>0.75 = stable):\n")
  print(round(boot_res$bootmean, 3))
  write_csv(
    tibble(cluster      = seq_along(boot_res$bootmean),
           mean_jaccard = boot_res$bootmean),
    "output/stability_bootstrap.csv"
  )
}

# ---------------------------------------------------------------------------
# Build the .gpkg 
# ---------------------------------------------------------------------------
out <- origins %>%
  inner_join(curves %>% select(origin_id, cluster), by = c("code" = "origin_id")) %>%
  inner_join(urban_default,                         by = c("code" = "origin_id"))

st_write(out, "output/urban_clusters.gpkg", delete_dsn = TRUE)
cat("Wrote output/urban_clusters.gpkg (", nrow(out), "polygons)\n")

# ---------------------------------------------------------------------------
# Cluster descriptors (shape only; numbered by near-home baseline, descending)
# ---------------------------------------------------------------------------
# Cluster 1 — "Near-home concentration"
#   High baseline at the slowest walking speed and rapid plateau: most
#   reachable services are already within reach of a slow walker, and walking
#   faster adds little.
#
# Cluster 2 — "Mid-distance concentration"
#   Moderate near-home baseline with most gains achieved at mid-range walking
#   speeds; returns diminish beyond a 7–9 minute walk, suggesting services
#   are concentrated within a moderate distance.
#
# Cluster 3 — "Gradual, well-served"
#   Smooth concave growth from a moderate baseline; services are spread fairly
#   evenly across the walking-speed range.
#
# Cluster 4 — "Mid-range linear"
#   Steady, approximately linear growth from a low baseline. Slow walkers are
#   penalised but not excluded, and each increment in walking speed adds a
#   roughly equal share of accessible services.
#
# Cluster 5 — "Far-only / sparse near home"
#   Lowest baseline and almost flat until the higher walking speeds, then a
#   steep increase. Slow walkers reach almost nothing; only faster walkers
#   gain meaningful access.

