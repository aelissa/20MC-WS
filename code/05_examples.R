# ---------------------------------------------------------------------------
# Worked examples: same 10-min default score, different typology cells
# ---------------------------------------------------------------------------

library(tidyverse)
library(sf)
library(cowplot)

# ---------------------------------------------------------------------------
# Load data
# ---------------------------------------------------------------------------
typology <- st_read("output/research_data/wact.gpkg", quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(
    cluster = factor(cluster),
    tertile = factor(tertile, levels = c("Low", "Mid", "High")),
    typology = factor(typology)
  )

results_long <- read_csv("routing_output/accessibility_all_themes_long.csv",
                         show_col_types = FALSE)
n_reach_cols <- grep("^n_reachable_", names(results_long), value = TRUE)

# Pick, within a default_score percentile band, one RANDOM origin per shape
# cluster (seeded for reproducibility), and build its reachability-curve
# panel (one subplot per cluster, shared y-axis within the band, since
# that's the comparison the panel is making). Candidates are restricted to
# origins with something reachable even at the slowest speed (30 m/min) --
# a principled filter applied to the whole candidate pool before sampling,
# rather than excluding specific origin_ids by hand -- so the 30->90 m/min
# multiplier is always defined.

worked_example_panel <- function(p_lo, p_hi, csv_path, seed = 42) {
  band_lo <- quantile(typology$default_score, p_lo, na.rm = TRUE)
  band_hi <- quantile(typology$default_score, p_hi, na.rm = TRUE)
  band    <- typology %>% filter(default_score >= band_lo, default_score <= band_hi)

  n_services_30_by_origin <- results_long %>%
    filter(origin_id %in% band$code) %>%
    group_by(origin_id) %>%
    summarise(n_services_30 = sum(n_reachable_5min, na.rm = TRUE), .groups = "drop")

  valid_ids <- n_services_30_by_origin %>% filter(n_services_30 > 0) %>% pull(origin_id)
  band <- band %>% filter(code %in% valid_ids)

  set.seed(seed)
  worked <- band %>%
    group_by(cluster) %>%
    slice_sample(n = 1) %>%
    ungroup() %>%
    arrange(cluster) %>%
    mutate(typology_label = factor(paste(cluster_label, tertile, sep = " · "),
                                   levels = paste(cluster_label, tertile, sep = " · ")[order(cluster)]))

  cat(sprintf("\nWorked example: %d origins with default_score in [%.2f, %.2f]\n",
              nrow(worked), band_lo, band_hi))
  print(worked %>% select(code, cluster, tertile, default_score, typology_label))
  write_csv(worked, csv_path)

  curves <- results_long %>%
    filter(origin_id %in% worked$code) %>%
    group_by(origin_id) %>%
    summarise(across(all_of(n_reach_cols), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>%
    pivot_longer(cols = all_of(n_reach_cols),
                 names_to  = "walking_col", values_to = "n_services") %>%
    mutate(walking_speed = as.integer(sub("n_reachable_(\\d+)min", "\\1", walking_col)) * 6) %>%
    inner_join(worked %>% select(code, cluster, typology_label, default_score),
               by = c("origin_id" = "code"))

  pct_at_default <- curves %>%
    filter(walking_speed %in% c(60, 90)) %>%
    select(origin_id, cluster, typology_label, walking_speed, n_services) %>%
    pivot_wider(names_from = walking_speed, values_from = n_services,
                names_prefix = "n_services_") %>%
    mutate(
      pct_at_default_speed = round(100 * n_services_60 / n_services_90, 1),
      pct_label = sprintf("%.0f%% of the destinations\naccessed at the fastest speed\nare reachable at the default speed", pct_at_default_speed)
    ) %>%
    arrange(cluster)

  # Fixed position (horizontal centre of the panel, top of the plot area)
  # rather than anchored to the curve's own endpoint, so the label is
  # centred and consistently placed across all panels regardless of how
  # high each curve happens to reach.
  label_data <- pct_at_default %>%
    select(origin_id, typology_label, pct_label) %>%
    mutate(walking_speed = 60)

  p <- ggplot(curves, aes(x = walking_speed, y = n_services, color = cluster)) +
    geom_line(linewidth = 1.4) +
    geom_point(size = 2) +
    geom_text(data = label_data, aes(x = walking_speed, y = Inf, label = pct_label),
              inherit.aes = FALSE, color = "grey20",
              hjust = 0.5, vjust = 1.4, size = 3.6,
              fontface = "bold", lineheight = 0.9) +
    facet_wrap(~ typology_label, ncol = 5, scales = "fixed",
               labeller = labeller(typology_label = label_wrap_gen(18))) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
    labs(x = "Walking speed (m/min)", y = "Services reachable (sum across themes)") +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 12)
    )

  list(plot = p, curves = curves, pct_at_default = pct_at_default)
}

high <- worked_example_panel(0.90, 0.95, "output/examples/worked_examples_high.csv")

low  <- worked_example_panel(0.10, 0.15, "output/examples/worked_examples_low.csv")

# Combined figure: panel A = high accessibility band, panel B = low
# accessibility band. Each panel keeps its own y-axis (the two bands differ
# by an order of magnitude in raw service counts, so a shared axis would
# flatten one of them) — stacked into one image rather than one shared
# facet grid for that reason. Bigger text + higher resolution than before,
# for legibility at publication size.
worked_examples_combined <- plot_grid(high$plot, low$plot, nrow = 2,
                                       labels = c("A", "B"),
                                       label_size = 24)
ggsave("output/examples/worked_examples.jpeg", worked_examples_combined,
       width = 20, height = 14, dpi = 500)
cat("Combined worked-examples figure written to output/examples/worked_examples.jpeg\n")


pct_at_default_table <- bind_rows(
  high$pct_at_default %>% mutate(band = "High", .before = 1),
  low$pct_at_default  %>% mutate(band = "Low", .before = 1)
) %>%
  select(band, origin_id, cluster, typology_label, n_services_60, n_services_90,
         pct_at_default_speed)

cat("\n% of eventual access already available at the default speed (60 m/min), per worked example:\n")
print(pct_at_default_table)
write_csv(pct_at_default_table, "output/examples/worked_examples_pct_at_default.csv")
cat("\nWritten to output/examples/worked_examples_pct_at_default.csv\n")
