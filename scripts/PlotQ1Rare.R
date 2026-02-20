#GST Plot with Rarefied as well

# TO PLOT: ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)

subsite_table <- readRDS("output/diversitymetrics_table_rare.rds")

# Define the metrics for the plot
metrics <- c("plantarea_ha", "tree_count", "tree_density_plant_ha", "Cov_rich", "Cov_q1", "Cov_pie")

# Summarize mean and standard error by GreenSpace for each metric
summary_gs <- subsite_table %>%
  group_by(GreenSpace) %>%
  summarise(across(all_of(metrics),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                   .names = "{col}.{fn}"),
            .groups = "drop")

# Prepare the subsite data in long format
long_subsites <- subsite_table %>%
  select(GreenSpace, all_of(metrics)) %>%
  pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "value")

# Prepare the summary data in long format
long_summary <- summary_gs %>%
  pivot_longer(cols = -GreenSpace,
               names_to = c("metric", "stat"),
               names_sep = "\\.",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

# Join summary stats to subsites and order GreenSpace by mean metric value per metric
plot_data <- long_subsites %>%
  left_join(long_summary, by = c("GreenSpace", "metric")) %>%
  group_by(metric) %>%
  mutate(GreenSpace_ordered = factor(GreenSpace, levels = unique(GreenSpace[order(mean)]))) %>%
  ungroup()

# Define pretty labels for the facets
metric_labels <- c(
  plantarea_ha = "Plantable Area (ha)",
  tree_count = "Tree Abundance",
  tree_density_plant_ha = "Tree Density (per ha)",
  Cov_rich = "Species Richness",
  Cov_q1 = "Effective Number of Species (q=1)",
  Cov_pie = "Evenness (Hurlbert's PIE)"
)

# Fixed order for GreenSpace types in the plot
ordered_levels <- c("Commercial", "Vacant Lot", "Park", "Institutional", "Public Right-Of-Way", "Residential")

# Make factors use that fixed order
plot_data <- plot_data %>%
  mutate(GreenSpace = factor(GreenSpace, levels = ordered_levels),
         GreenSpace_ordered = factor(GreenSpace_ordered, levels = ordered_levels))

# Define ordered factor levels for metric (controls facet order)
metric_levels_ordered <- c(
  "plantarea_ha",
  "tree_count",
  "tree_density_plant_ha",
  "Cov_rich",
  "Cov_q1",
  "Cov_pie"
)

# Make metric a factor with this order in plot_data and long_summary
plot_data$metric <- factor(plot_data$metric, levels = metric_levels_ordered)
long_summary$metric <- factor(long_summary$metric, levels = metric_levels_ordered)

# Update labels with letters as you want
metric_labels_tagged <- c(
  plantarea_ha = "A) Plantable Area (ha)",
  tree_count = "B) Tree Abundance",
  tree_density_area_ha = "C) Tree Density (per ha)",
  Cov_rich = "D) Species Richness",
  Cov_q1 = "E) Effective Number of Species",
  Cov_pie = "F) Evenness (Hurlbert's PIE)"
)

# Plot with fixed order and 2 rows, 3 columns
ggplot() +
  geom_jitter(data = plot_data,
              aes(x = GreenSpace_ordered, y = value),
              width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = long_summary %>% mutate(GreenSpace = factor(GreenSpace, levels = ordered_levels)),
             aes(x = GreenSpace, y = mean, color = GreenSpace),
             size = 3) +
  geom_errorbar(data = long_summary %>% mutate(GreenSpace = factor(GreenSpace, levels = ordered_levels)),
                aes(x = GreenSpace, ymin = mean - se, ymax = mean + se, color = GreenSpace),
                width = 0.5, linewidth = 1) +
  facet_wrap(~ metric, scales = "free_y", 
             labeller = labeller(metric = metric_labels_tagged),
             nrow = 2, ncol = 3) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    strip.text = element_text(size = 12)
  ) +
  labs(x = "Green Space Type", y = "Metric Value", colour = "Green Space Type")

