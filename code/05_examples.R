# ---------------------------------------------------------------------------
# Worked examples: same 10-min score, different typology cells
# ---------------------------------------------------------------------------
# Take a narrow score band near the median; pick one origin per cluster within
# it. Five areas with near-identical 10-min scores that get classified
# differently by the typology.

library(tidyverse)
library(sf)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
typology <- st_read("output/20MN_typology.gpkg", quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(
    cluster       = factor(cluster),
    magnitude_bin = factor(magnitude_bin, levels = c("Low", "Mid", "High")),
    typology      = factor(typology)
  )

results_long <- read_csv("routing_output/accessibility_all_themes_long.csv",
                         show_col_types = FALSE)
n_reach_cols <- grep("^n_reachable_", names(results_long), value = TRUE)


#####high#####
band_lo <- quantile(typology$default_score, 0.90, na.rm = TRUE)
band_hi <- quantile(typology$default_score, 0.95, na.rm = TRUE)
band    <- typology %>% filter(default_score >= band_lo, default_score <= band_hi)
center  <- median(band$default_score, na.rm = TRUE)

worked <- band %>%
  group_by(cluster) %>%
  slice_min(abs(default_score - center), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(cluster)

cat(sprintf("\nWorked example: %d origins with default_score in [%.2f, %.2f]\n",
            nrow(worked), band_lo, band_hi))
print(worked %>% select(code, cluster, magnitude_bin, default_score, typology))
write_csv(worked, "output/examples/worked_examples_high.csv")

curves <- results_long %>%
  filter(origin_id %in% worked$code) %>%
  group_by(origin_id) %>%
  summarise(across(all_of(n_reach_cols), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") %>%
  pivot_longer(cols = all_of(n_reach_cols),
               names_to  = "walking_col", values_to = "n_services") %>%
  mutate(walking_speed = as.integer(sub("n_reachable_(\\d+)min", "\\1", walking_col)) * 6) %>%
  inner_join(worked %>% select(code, cluster, typology, default_score),
             by = c("origin_id" = "code"))

p_worked <- ggplot(curves,
                   aes(x = walking_speed, y = n_services, color = cluster)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  facet_wrap(~ typology, ncol = 5, scales = "fixed") +
  labs(x = "Walking speed (m/min)", y = "Services reachable (sum across themes)") +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(size = 8))
ggsave("output/examples/worked_examples_high.jpeg", p_worked,
       width = 14, height = 5, dpi = 300)

####low band###

band_lo <- quantile(typology$default_score, 0.10, na.rm = TRUE)
band_hi <- quantile(typology$default_score, 0.15, na.rm = TRUE)
band    <- typology %>% filter(default_score >= band_lo, default_score <= band_hi)
center  <- median(band$default_score, na.rm = TRUE)

worked <- band %>%
  group_by(cluster) %>%
  slice_min(abs(default_score - center), n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(cluster)

cat(sprintf("\nWorked example: %d origins with default_score in [%.2f, %.2f]\n",
            nrow(worked), band_lo, band_hi))
print(worked %>% select(code, cluster, magnitude_bin, default_score, typology))
write_csv(worked, "output/examples/worked_examples_low.csv")

curves <- results_long %>%
  filter(origin_id %in% worked$code) %>%
  group_by(origin_id) %>%
  summarise(across(all_of(n_reach_cols), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop") %>%
  pivot_longer(cols = all_of(n_reach_cols),
               names_to  = "walking_col", values_to = "n_services") %>%
  mutate(walking_speed = as.integer(sub("n_reachable_(\\d+)min", "\\1", walking_col)) * 6) %>%
  inner_join(worked %>% select(code, cluster, typology, default_score),
             by = c("origin_id" = "code"))

p_worked <- ggplot(curves,
                   aes(x = walking_speed, y = n_services, color = cluster)) +
  geom_line(linewidth = 1.2) +
  geom_point() +
  facet_wrap(~ typology, ncol = 5, scales = "fixed") +
  labs(x = "Walking speed (m/min)", y = "Services reachable (sum across themes)") +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(size = 8))
ggsave("output/examples/worked_examples_low.jpeg", p_worked,
       width = 14, height = 5, dpi = 300)

