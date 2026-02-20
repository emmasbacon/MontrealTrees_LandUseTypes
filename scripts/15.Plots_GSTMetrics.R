#Ch1 GST Plots Metrics

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbreak)
library(ggpubr)

subsite_table <- readRDS("output/diversitymetrics_table_rare.rds")

# Fixed order for GSTs
ordered_levels <- c("Commercial", "Vacant Lot", "Park", "Institutional", 
                    "Public Right-Of-Way", "Residential")

# GST colors
gs_colors <- c(
  "Commercial"           = "#F8766D",
  "Vacant Lot"           = "#B79F00",
  "Park"                 = "#00BA38",
  "Institutional"        = "#00BFC4",
  "Public Right-Of-Way"  = "#619CFF",
  "Residential"          = "#F564E3"
)

# A helper function to make summary + plot for selected metrics
make_metric_plot <- function(metrics, metric_labels_tagged, nrow, ncol) {
  
  # Summarise
  summary_gs <- subsite_table %>%
    group_by(GreenSpace) %>%
    summarise(across(all_of(metrics),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                     .names = "{col}.{fn}"),
              .groups = "drop")
  
  # Subsites long
  long_subsites <- subsite_table %>%
    select(GreenSpace, all_of(metrics)) %>%
    pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "value")
  
  # Summary long
  long_summary <- summary_gs %>%
    pivot_longer(cols = -GreenSpace,
                 names_to = c("metric", "stat"),
                 names_sep = "\\.",
                 values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value)
  
  # Join
  plot_data <- long_subsites %>%
    left_join(long_summary, by = c("GreenSpace", "metric")) %>%
    group_by(metric) %>%
    mutate(GreenSpace_ordered = factor(GreenSpace, 
                                       levels = unique(GreenSpace[order(mean)]))) %>%
    ungroup()
  
  # Ensure fixed order
  plot_data <- plot_data %>%
    mutate(GreenSpace = factor(GreenSpace, levels = ordered_levels),
           GreenSpace_ordered = factor(GreenSpace_ordered, levels = ordered_levels))
  long_summary$metric <- factor(long_summary$metric, levels = metrics)
  plot_data$metric <- factor(plot_data$metric, levels = metrics)
  
  # Plot
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
               nrow = nrow, ncol = ncol) +
    scale_color_manual(values = gs_colors) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      legend.position = "right",
      strip.text = element_text(size = 12)
    ) +
    labs(x = "Land Use Type", y = "Metric Value", colour = "Land Use Type")
}

#PLOT 1: AREA PLOT

metrics_area <- c("area_ha", "plantarea_ha")
metric_labels_area <- c(
  area_ha = "A) Total Area (ha)",
  plantarea_ha = "B) Plantable Area (ha)"
)

p_area <- make_metric_plot(metrics_area, metric_labels_area, nrow = 1, ncol = 2)

p_area

#PLOT 2: BASAL AREA PLOT

metrics_ba <- c("basal_area_ha", "basal_area_plant_ha")
metric_labels_ba <- c(
  basal_area_ha = "A) Basal Area (m²/ha)",
  basal_area_plant_ha = "B) Basal Area (m²/plantable ha)"
)

p_ba <- make_metric_plot(metrics_ba, metric_labels_ba, nrow = 1, ncol = 2)

p_ba

#PLOT 3: COUNT AND DIVERSITY PLOT

metrics_div <- c("tree_count", "Cov_rich", "Cov_q1", "Cov_pie")
metric_labels_div <- c(
  tree_count = "A) Tree Abundance",
  Cov_rich   = "B) Species Richness",
  Cov_q1     = "C) Effective Number of Species (q=1)",
  Cov_pie    = "D) Evenness (Hurlbert's PIE)"
)

p_div <- make_metric_plot(metrics_div, metric_labels_div, nrow = 2, ncol = 2)

p_div

#PLOT 4 OPTION: BASAL AREA, COUNT, DIVERSITY PLOT

metrics_combined <- c("tree_count", "Cov_rich", "Cov_q1", "Cov_pie", 
                      "basal_area_ha", "basal_area_plant_ha")

metric_labels_combined <- c(
  tree_count           = "A) Tree Abundance",
  Cov_rich             = "B) Species Richness",
  Cov_q1               = "C) Effective Number of Species (q=1)",
  Cov_pie              = "D) Evenness (Hurlbert's PIE)",
  basal_area_ha        = "E) Basal Area per ha (subsite)",
  basal_area_plant_ha  = "F) Basal Area per ha (plantable)"
)

p_combined <- make_metric_plot(metrics_combined, metric_labels_combined, 
                               nrow = 2, ncol = 3)
p_combined

