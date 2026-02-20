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





#TO TRY
library(ggplot2)
library(ggpubr)
library(dplyr)

subsite_table <- readRDS("output/diversitymetrics_table_rare.rds")

ordered_levels <- c("Commercial", "Vacant Lot", "Park", "Institutional",
                    "Public Right-Of-Way", "Residential")

gs_colors <- c(
  "Commercial"           = "#F8766D",
  "Vacant Lot"           = "#B79F00",
  "Park"                 = "#00BA38",
  "Institutional"        = "#00BFC4",
  "Public Right-Of-Way"  = "#619CFF",
  "Residential"          = "#F564E3"
)

#------------------------
# PLOT 1: AREA
#------------------------
# Total Area
summary_area <- subsite_table %>%
  group_by(GreenSpace) %>%
  summarise(mean = mean(area_ha, na.rm = TRUE),
            se   = sd(area_ha, na.rm = TRUE)/sqrt(sum(!is.na(area_ha))),
            .groups = "drop")

p_area_ha <- ggplot(subsite_table, aes(x = factor(GreenSpace, levels = ordered_levels),
                                       y = area_ha)) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = summary_area, aes(y = mean, color = GreenSpace), size = 3) +
  geom_errorbar(data = summary_area, aes(ymin = mean - se, ymax = mean + se, color = GreenSpace),
                width = 0.5, linewidth = 1) +
  scale_color_manual(values = gs_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Green Space Type", y = "Total Area (ha)", color = "Green Space Type", title = "A) Total Area (ha)")

# Plantable Area
summary_plant <- subsite_table %>%
  group_by(GreenSpace) %>%
  summarise(mean = mean(plantarea_ha, na.rm = TRUE),
            se   = sd(plantarea_ha, na.rm = TRUE)/sqrt(sum(!is.na(plantarea_ha))),
            .groups = "drop")

p_plantarea_ha <- ggplot(subsite_table, aes(x = factor(GreenSpace, levels = ordered_levels),
                                            y = plantarea_ha)) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = summary_plant, aes(y = mean, color = GreenSpace), size = 3) +
  geom_errorbar(data = summary_plant, aes(ymin = mean - se, ymax = mean + se, color = GreenSpace),
                width = 0.5, linewidth = 1) +
  scale_color_manual(values = gs_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Green Space Type", y = "Plantable Area (ha)", color = "Green Space Type", title = "B) Plantable Area (ha)")

p_area <- ggarrange(p_area_ha, p_plantarea_ha, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")

#------------------------
# PLOT 2: BASAL AREA
# Manual break for basal_area_plant_ha: 80 to 140
#------------------------
summary_ba_ha <- subsite_table %>%
  group_by(GreenSpace) %>%
  summarise(mean = mean(basal_area_ha, na.rm = TRUE),
            se   = sd(basal_area_ha, na.rm = TRUE)/sqrt(sum(!is.na(basal_area_ha))),
            .groups = "drop")

p_ba_ha <- ggplot(subsite_table, aes(x = factor(GreenSpace, levels = ordered_levels),
                                     y = basal_area_ha)) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = summary_ba_ha, aes(y = mean, color = GreenSpace), size = 3) +
  geom_errorbar(data = summary_ba_ha, aes(ymin = mean - se, ymax = mean + se, color = GreenSpace),
                width = 0.5, linewidth = 1) +
  scale_color_manual(values = gs_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Green Space Type", y = "Basal Area per ha", color = "Green Space Type", title = "A) Basal Area per ha")

summary_ba_plant <- subsite_table %>%
  group_by(GreenSpace) %>%
  summarise(mean = mean(basal_area_plant_ha, na.rm = TRUE),
            se   = sd(basal_area_plant_ha, na.rm = TRUE)/sqrt(sum(!is.na(basal_area_plant_ha))),
            .groups = "drop")

p_ba_plant_ha <- ggplot(subsite_table, aes(x = factor(GreenSpace, levels = ordered_levels),
                                           y = basal_area_plant_ha)) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = summary_ba_plant, aes(y = mean, color = GreenSpace), size = 3) +
  geom_errorbar(data = summary_ba_plant, aes(ymin = mean - se, ymax = mean + se, color = GreenSpace),
                width = 0.5, linewidth = 1) +
  scale_color_manual(values = gs_colors) +
  scale_y_continuous(breaks = c(seq(0,80,20), 140, max(subsite_table$basal_area_plant_ha, na.rm = TRUE))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Green Space Type", y = "Basal Area per Plantable ha", color = "Green Space Type", title = "B) Basal Area per Plantable ha")

p_ba <- ggarrange(p_ba_ha, p_ba_plant_ha, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")

#------------------------
# PLOT 3: TREE COUNT & DIVERSITY
# Manual breaks:
# tree_count: 1620-2220
# Cov_rich: 100-180
#------------------------
summary_tree <- subsite_table %>%
  group_by(GreenSpace) %>%
  summarise(mean = mean(tree_count, na.rm = TRUE),
            se   = sd(tree_count, na.rm = TRUE)/sqrt(sum(!is.na(tree_count))),
            .groups = "drop")

p_tree <- ggplot(subsite_table, aes(x = factor(GreenSpace, levels = ordered_levels),
                                    y = tree_count)) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = summary_tree, aes(y = mean, color = GreenSpace), size = 3) +
  geom_errorbar(data = summary_tree, aes(ymin = mean - se, ymax = mean + se, color = GreenSpace),
                width = 0.5, linewidth = 1) +
  scale_color_manual(values = gs_colors) +
  scale_y_continuous(breaks = c(seq(0,1620,400), 2220, max(subsite_table$tree_count, na.rm = TRUE))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Green Space Type", y = "Tree Abundance", color = "Green Space Type", title = "A) Tree Abundance")

summary_rich <- subsite_table %>%
  group_by(GreenSpace) %>%
  summarise(mean = mean(Cov_rich, na.rm = TRUE),
            se   = sd(Cov_rich, na.rm = TRUE)/sqrt(sum(!is.na(Cov_rich))),
            .groups = "drop")

p_rich <- ggplot(subsite_table, aes(x = factor(GreenSpace, levels = ordered_levels),
                                    y = Cov_rich)) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = summary_rich, aes(y = mean, color = GreenSpace), size = 3) +
  geom_errorbar(data = summary_rich, aes(ymin = mean - se, ymax = mean + se, color = GreenSpace),
                width = 0.5, linewidth = 1) +
  scale_color_manual(values = gs_colors) +
  scale_y_continuous(breaks = c(seq(0,100,20), 180, max(subsite_table$Cov_rich, na.rm = TRUE))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Green Space Type", y = "Species Richness", color = "Green Space Type", title = "B) Species Richness")

p_q1 <- ggplot(subsite_table, aes(x = factor(GreenSpace, levels = ordered_levels), y = Cov_q1)) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = subsite_table %>% group_by(GreenSpace) %>% summarise(mean = mean(Cov_q1)),
             aes(y = mean, color = GreenSpace), size = 3) +
  geom_errorbar(data = subsite_table %>% group_by(GreenSpace) %>% summarise(mean = mean(Cov_q1),
                                                                            se = sd(Cov_q1)/sqrt(n())),
                aes(ymin = mean - se, ymax = mean + se, color = GreenSpace), width = 0.5, linewidth = 1) +
  scale_color_manual(values = gs_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Green Space Type", y = "Effective Number of Species (q=1)", color = "Green Space Type", title = "C) Effective Number of Species (q=1)")

p_pie <- ggplot(subsite_table, aes(x = factor(GreenSpace, levels = ordered_levels), y = Cov_pie)) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = subsite_table %>% group_by(GreenSpace) %>% summarise(mean = mean(Cov_pie)),
             aes(y = mean, color = GreenSpace), size = 3) +
  geom_errorbar(data = subsite_table %>% group_by(GreenSpace) %>% summarise(mean = mean(Cov_pie),
                                                                            se = sd(Cov_pie)/sqrt(n())),
                aes(ymin = mean - se, ymax = mean + se, color = GreenSpace), width = 0.5, linewidth = 1) +
  scale_color_manual(values = gs_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Green Space Type", y = "Evenness (Hurlbert's PIE)", color = "Green Space Type", title = "D) Evenness (Hurlbert's PIE)")

p_div <- ggarrange(p_tree, p_rich, p_q1, p_pie, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right")

