library(tidyverse)
library(sf)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------

accessibility <- read_csv("output/research_data/accessibility_walkspeeds.csv",
                           show_col_types = FALSE)
origins       <- st_read("ancillary_data/origins_bnd.gpkg", quiet = TRUE)

# Settlement origin_ids only (UR3Class 1 = "Rest of Scotland")
settl_ids <- accessibility %>%
  filter(UR3Class == 1) %>%
  distinct(origin_id) %>%
  pull(origin_id)


results_long <- accessibility %>%
  mutate(minutes = as.integer(walking_speed_m_per_min / 6)) %>%
  select(origin_id, framework_theme, minutes, n_reachable) %>%
  pivot_wider(names_from = minutes, values_from = n_reachable,
              names_glue = "n_reachable_{minutes}min")

# ---------------------------------------------------------------------------
# Build normalised curves
# ---------------------------------------------------------------------------
n_reach_cols <- grep("^n_reachable_", names(results_long), value = TRUE)

curves <- results_long %>%
  filter(origin_id %in% settl_ids) %>%
  group_by(origin_id) %>%
  summarise(across(all_of(n_reach_cols), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop")

curves_mat  <- as.matrix(curves[, n_reach_cols])
row_max     <- apply(curves_mat, 1, max)
keep        <- row_max > 0                                 # drop origins with all zeros

dir.create("output/cluster_validation", showWarnings = FALSE)

# Zero/low-accessibility handling: row-normalisation divides by each
# origin's own row_max, which is undefined (0/0) for origins with zero
# destinations reachable at any walking speed, in any theme — those are
# dropped from clustering entirely (can't give them a shape). Among the
# ones kept, the row_max distribution below shows how much resolution a
# smaller row_max leaves for the normalised curve — there's no natural
# cutoff between "fine" and "coarse", just a continuum. Computed on
# row_max/keep before either is touched by the filtering below.

zero_accessibility_stats <- tibble(
  total_settlement_origins = length(keep),
  dropped_all_zero         = sum(!keep),
  pct_dropped              = round(mean(!keep) * 100, 2),
  kept                     = sum(keep),
  min_row_max              = min(row_max[keep]),
  q1_row_max               = quantile(row_max[keep], 0.25),
  median_row_max           = median(row_max[keep]),
  mean_row_max             = round(mean(row_max[keep]), 1),
  q3_row_max               = quantile(row_max[keep], 0.75),
  max_row_max              = max(row_max[keep])
)
write_csv(zero_accessibility_stats, "output/cluster_validation/zero_accessibility_stats.csv")

curves      <- curves[keep, ]
curves_mat  <- curves_mat[keep, ]
curves_norm <- curves_mat / row_max[keep]

cat("Origins clustered:", nrow(curves_norm), "\n")

# ---------------------------------------------------------------------------
# Clustergram (k = 2..8) — Galili's implementation in code/clustergram.R
# ---------------------------------------------------------------------------
source("code/clustergram.R")
set.seed(42)

jpeg("output/cluster_validation/clustergram.jpeg", width = 1400, height = 900, res = 110)
par(cex.lab = 1.2, cex.main = 0.9)
clustergram(curves_norm, k.range = 2:8, line.width = 0.00001, add.center.points = TRUE)
dev.off()
cat("Clustergram written to output/cluster_validation/clustergram.jpeg\n")

# ---------------------------------------------------------------------------
# Elbow plot (k = 2..8) — total within-cluster sum of squares, full data
# ---------------------------------------------------------------------------
set.seed(42)
k_range <- 2:8
wss <- sapply(k_range, function(k) {
  kmeans(curves_norm, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

p_elbow <- ggplot(tibble(k = k_range, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Elbow plot", x = "k", y = "Total within-cluster sum of squares")
ggsave("output/cluster_validation/elbow_plot.jpeg", p_elbow, width = 7, height = 5, dpi = 300)
cat("Elbow plot written to output/cluster_validation/elbow_plot.jpeg\n")

# ---------------------------------------------------------------------------
# Silhouette plot (k = 2..8) — average silhouette width on a 3000 random points
# subsample 
# ---------------------------------------------------------------------------
set.seed(42)
sil_sample <- curves_norm[sample(nrow(curves_norm), min(3000, nrow(curves_norm))), ]
sil_dist   <- dist(sil_sample)

sil_width <- sapply(k_range, function(k) {
  km_k <- kmeans(sil_sample, centers = k, nstart = 25, iter.max = 100)
  mean(cluster::silhouette(km_k$cluster, sil_dist)[, 3])
})

# k-selection stats table (elbow + silhouette together), computed here before
# the final clustering fit below settles on K.
write_csv(
  tibble(k = k_range, tot_withinss = wss, avg_silhouette = sil_width),
  "output/cluster_validation/k_selection_stats.csv"
)

p_sil <- ggplot(tibble(k = k_range, avg_silhouette = sil_width), aes(x = k, y = avg_silhouette)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Average silhouette width (n = 3,000 subsample)", x = "k", y = "Average silhouette width")
ggsave("output/cluster_validation/silhouette_plot.jpeg", p_sil, width = 7, height = 5, dpi = 300)
cat("Silhouette plot written to output/cluster_validation/silhouette_plot.jpeg\n")

# ---------------------------------------------------------------------------
# Combined k-selection figure — A = clustergram (top row, full width),
# B = elbow plot and C = silhouette plot (bottom row, side by side) — for
# use as a single paper figure. Composed from the three JPEGs just written,
# via magick, since the clustergram is a base-R plot and can't be combined
# with the ggplot panels as grobs directly.
# ---------------------------------------------------------------------------
library(magick)

panel_label <- function(img, label) {
  image_annotate(img, label, size = 70, weight = 700, color = "black",
                  location = "+20+10", gravity = "northwest")
}

img_b <- panel_label(image_read("output/cluster_validation/elbow_plot.jpeg"), "B")
img_c <- panel_label(image_read("output/cluster_validation/silhouette_plot.jpeg"), "C")
row_height <- 900
img_b <- image_scale(img_b, paste0("x", row_height))
img_c <- image_scale(img_c, paste0("x", row_height))
bottom_row <- image_append(c(img_b, img_c))

img_a <- panel_label(image_read("output/cluster_validation/clustergram.jpeg"), "A")
bottom_width <- image_info(bottom_row)$width
img_a <- image_scale(img_a, paste0(bottom_width, "x"))

k_selection_combined <- image_append(c(img_a, bottom_row), stack = TRUE)
image_write(k_selection_combined, "output/cluster_validation/k_selection_diagnostics.jpeg")
cat("Combined k-selection figure (A/B/C) written to output/cluster_validation/k_selection_diagnostics.jpeg\n")

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
  "2" = "Short-range coverage",
  "3" = "Gradually widening access",
  "4" = "Long-range access increase",
  "5" = "Far-range concentration"
)
write_csv(tibble(cluster = names(cluster_labels), label = unname(cluster_labels)),
          "output/cluster_labels.csv")

# Profiles descriptions matching the "Cluster descriptors" 

cluster_descriptions <- c(
  "1" = "Walking speed makes relatively little difference here: a good share of what's reachable with a brisk walk - 38% - is already available even at a slow pace, rising to 78% by the time someone reaches the default speed, with each further increase in speed adding progressively less.",
  "2" = "Access builds up at a fairly steady pace across the whole range of walking speeds, with only a slight slowdown towards faster walk speeds: about 62% of the services reachable with a brisk walk is available by the default speed, with the remaining 38% becoming accessible only by walking faster.",
  "3" = "Access builds up at a constant pace across the whole range of walking speeds, picking up slightly as speed increases: around half of what is reachable with a brisk walk is available up to the default speed, with the other 51% gained beyond it.",
  "4" = "Starts out quite flat at slower walking speeds - only 7% of the access to service for a fast walker is reached at the slowest pace - and still only about a third (35%) is available by the default speed; nearly two-thirds of all gains (65%) come only once someone can walk faster.",
  "5" = "Almost completely flat at slower walking speeds - barely 4% of the services accessible for a fast walker is reached at the slowest pace, and still under a fifth (19%) by the default speed - then rises near the fastest speeds, where over 80% of all gains are concentrated."
)

# Reorder centroid rows to match new semantic cluster numbers (needed below
# for the profile table, and again further down for the centroids plot).
centers_remapped <- km$centers[match(seq_len(K), remap), ]

# % of each cluster member's eventual total access (normalised centroid,
# so magnitude-independent) reached at the slowest speed, and gained in
# each half of the walking-speed range. These three always sum to ~100%.

pct_reached_at_30 <- round(centers_remapped[, 1] * 100, 1)
pct_gained_30_to_60 <- round((centers_remapped[, 6] - centers_remapped[, 1]) * 100, 1)
pct_gained_60_to_90 <- round((centers_remapped[, 11] - centers_remapped[, 6]) * 100, 1)

cluster_sizes <- table(curves$cluster)
cluster_profile <- tibble(
  cluster = names(cluster_labels),
  label = unname(cluster_labels),
  n = as.integer(cluster_sizes[names(cluster_labels)]),
  pct_of_settlements = round(n / sum(n) * 100, 1),
  pct_reached_at_30 = pct_reached_at_30,
  pct_gained_30_to_60 = pct_gained_30_to_60,
  pct_gained_60_to_90 = pct_gained_60_to_90,
  description = unname(cluster_descriptions[names(cluster_labels)])
)
write_csv(cluster_profile, "output/cluster_validation/cluster_profile.csv")

# ---------------------------------------------------------------------------
# Cluster centroids plot — Data Overview figure, legend labelled by cluster
# name (not number)
# ---------------------------------------------------------------------------
centroids <- as_tibble(centers_remapped) %>%
  mutate(
    cluster       = factor(seq_len(K)),
    cluster_label = factor(cluster_labels[as.character(cluster)], levels = cluster_labels)
  ) %>%
  pivot_longer(cols = all_of(n_reach_cols),
               names_to = "walking_col", values_to = "norm_value") %>%
  mutate(
    minutes       = as.integer(sub("n_reachable_(\\d+)min", "\\1", walking_col)),
    walking_speed = minutes * 6
  )

write_csv(centroids %>% select(cluster, cluster_label, walking_speed, norm_value),
          "output/cluster_validation/cluster_centroids.csv")

# % of eventual access already gained by the default speed (60 m/min),
# labelled directly on the curve instead of a legend lookup.
pct_at_60 <- centroids %>%
  filter(walking_speed == 60) %>%
  mutate(pct_label = paste0(round(norm_value * 100, 1), "%"))

# White halo behind the % labels: 8 white copies nudged in a ring behind
# the coloured text on top (no extra package required).
halo_offsets <- expand.grid(dx = c(-0.6, 0, 0.6), dy = c(-0.012, 0, 0.012))
halo_offsets <- halo_offsets[!(halo_offsets$dx == 0 & halo_offsets$dy == 0), ]

p_centroids <- ggplot(centroids, aes(x = walking_speed, y = norm_value,
                                      color = cluster_label, group = cluster_label)) +
  geom_line(linewidth = 1)
for (i in seq_len(nrow(halo_offsets))) {
  p_centroids <- p_centroids +
    geom_text(data = pct_at_60, aes(label = pct_label),
              size = 3.5, fontface = "bold", color = "white", show.legend = FALSE,
              position = position_nudge(x = halo_offsets$dx[i], y = halo_offsets$dy[i]),
              vjust = -0.8)
}
p_centroids <- p_centroids +
  geom_text(data = pct_at_60, aes(label = pct_label), size = 3.5, fontface = "bold",
            show.legend = FALSE, vjust = -0.8) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(x = "Walking speed (m/min)",
       y = "Services reachable (% of the amount reachable at 90 m/min in 10-minute walk)",
       color = "Walking-speed accessibility profile")
ggsave("output/cluster_validation/cluster_centroids.jpeg", p_centroids,
       width = 8, height = 5.5, dpi = 300)
cat("Cluster centroids plot written to output/cluster_validation/cluster_centroids.jpeg\n")

# Companion table for the figure above: % of eventual access reached at the
# slowest speed, gained in each half of the speed range, and the cumulative
# % reached by 60 m/min (pct_reached_at_60 = pct_reached_at_30 +
# pct_gained_30_to_60) — this last one is the number labelled on the plot
# itself, kept here as its own column so it doesn't have to be added up by
# hand when reading the table alongside the figure.
cluster_centroid_slopes <- cluster_profile %>%
  mutate(pct_reached_at_60 = pct_reached_at_30 + pct_gained_30_to_60) %>%
  select(cluster, label, pct_reached_at_30, pct_reached_at_60,
         pct_gained_30_to_60, pct_gained_60_to_90)
write_csv(cluster_centroid_slopes, "output/cluster_validation/cluster_centroid_slopes.csv")

#### publish research data ####
# One row per settlement origin: its shape cluster and the semantic label
# for that cluster, joined in for readability.

publish_walking_speed_accessibility_profiles <- curves %>%
  select(origin_id, cluster) %>%
  mutate(cluster = as.character(cluster)) %>%
  left_join(
    tibble(cluster = names(cluster_labels), cluster_label = unname(cluster_labels)),
    by = "cluster"
  ) %>%
  arrange(origin_id)

write_csv(publish_walking_speed_accessibility_profiles, "output/research_data/walking-speed_accessibility_profiles.csv")

# ---------------------------------------------------------------------------
# Cluster validation — stability under re-seeding + bootstrap stability
# ---------------------------------------------------------------------------
# Requires: mclust (for Adjusted Rand Index), fpc (for clusterboot)
# Expensive (100 re-seeds x2 + 100 bootstrap resamples) — set to FALSE to
# skip when only the clustering/diagnostic-plot steps above need a rerun.
RUN_STABILITY_CHECKS <- TRUE

if (RUN_STABILITY_CHECKS) {

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
            "output/cluster_validation/stability_reseed.csv")

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
    "output/cluster_validation/stability_reseed_nstart1.csv"
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
    "output/cluster_validation/stability_bootstrap.csv"
  )
}

} else {
  cat("\nRUN_STABILITY_CHECKS is FALSE — skipping re-seed/bootstrap stability.\n")
}

# ---------------------------------------------------------------------------
# Build the .gpkg 
# ---------------------------------------------------------------------------
out <- origins %>%
  inner_join(curves %>% select(origin_id, cluster), by = c("code" = "origin_id"))

st_write(out, "output/walking-speed_accessibility_profiles.gpkg", delete_dsn = TRUE)
cat("Wrote output/walking-speed_accessibility_profiles.gpkg (", nrow(out), "polygons)\n")

# ---------------------------------------------------------------------------
# Cluster descriptors (shape only; numbered by near-home baseline, descending)
# ---------------------------------------------------------------------------
# Cluster 1 — "Near-home concentration"
#   Walking speed makes relatively little difference here: a good share
#   of what's reachable with a brisk walk — 38% — is already available
#   even at a slow pace, rising to 78% by the time someone reaches the
#   default speed, with each further increase in speed adding
#   progressively less.
#
# Cluster 2 — "Short-range coverage"
#   Access builds up at a fairly steady pace across the whole range of
#   walking speeds, with only a slight slowdown towards faster walk
#   speeds: about 62% of the services reachable with a brisk walk is
#   available by the default speed, with the remaining 38% becoming
#   accessible only by walking faster.
#
# Cluster 3 — "Gradually widening access"
#   Access builds up at a constant pace across the whole range of
#   walking speeds, picking up slightly as speed increases: around half
#   of what is reachable with a brisk walk is available up to the
#   default speed, with the other 51% gained beyond it.
#
# Cluster 4 — "Long-range access increase"
#   Starts out quite flat at slower walking speeds — only 7% of the
#   access to service for a fast walker is reached at the slowest pace —
#   and still only about a third (35%) is available by the default
#   speed; nearly two-thirds of all gains (65%) come only once someone
#   can walk faster.
#
# Cluster 5 — "Far-range concentration"
#   Almost completely flat at slower walking speeds — barely 4% of the
#   services accessible for a fast walker is reached at the slowest
#   pace, and still under a fifth (19%) by the default speed — then
#   rises near the fastest speeds, where over 80% of all gains are
#   concentrated.

