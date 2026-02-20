#GST Q1 Plot

library(dplyr)
library(ggplot2)

subsite_table <- readRDS("output/subsite_metrics.rds")

#Mean by green space type
#I havent decide what metrics I want to include so I am making three different versions for now

metrics1 <- c("area_ha", "tree_count", "tree_density_ha", "richness", "hill_q1", "pie_eve")
metrics2 <- c("area_ha", "tree_count", "tree_density_ha", "richness", "hill_q1", "hill_q2")
metrics3 <- c("area_ha", "tree_count", "tree_density_ha", "richness", "shannon", "pie_eve")

get_summary_by_gst <- function(data, metrics) {
  data %>%
    group_by(GreenSpace) %>%
    summarise(across(all_of(metrics),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          se = ~sd(.x, na.rm = TRUE) / sqrt(sum(!is.na(.x)))),
                     .names = "{col}.{fn}"),
              .groups = "drop")
}

summary_gs1 <- get_summary_by_gst(subsite_table, metrics1)
summary_gs2 <- get_summary_by_gst(subsite_table, metrics2)
summary_gs3 <- get_summary_by_gst(subsite_table, metrics3)

prepare_long_data <- function(subsite_df, summary_df, metrics) {
  
  long_subsites <- subsite_df %>%
    select(GreenSpace, all_of(metrics)) %>%
    pivot_longer(cols = all_of(metrics), names_to = "metric", values_to = "value")
  
  long_summary <- summary_df %>%
    pivot_longer(cols = -GreenSpace,
                 names_to = c("metric", "stat"),
                 names_sep = "\\.",
                 values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value)
  
  list(long_subsites = long_subsites, long_summary = long_summary)
}

reorder_greenspace_by_metric <- function(long_subsites, long_summary) {
  long_subsites %>%
    left_join(long_summary, by = c("GreenSpace", "metric")) %>%
    group_by(metric) %>%
    mutate(GreenSpace_ordered = factor(GreenSpace, levels = unique(GreenSpace[order(mean)]))) %>%
    ungroup()
}

plot_data1_list <- prepare_long_data(subsite_table, summary_gs1, metrics1)
plot_data1_list$long_subsites$metric <- factor(plot_data1_list$long_subsites$metric, levels = metrics1)
plot_data1_list$long_summary$metric <- factor(plot_data1_list$long_summary$metric, levels = metrics1)
plot_data1_df <- reorder_greenspace_by_metric(plot_data1_list[[1]], plot_data1_list[[2]])

plot_data2_list <- prepare_long_data(subsite_table, summary_gs2, metrics2)
plot_data2_list$long_subsites$metric <- factor(plot_data2_list$long_subsites$metric, levels = metrics2)
plot_data2_list$long_summary$metric <- factor(plot_data2_list$long_summary$metric, levels = metrics2)
plot_data2_df <- reorder_greenspace_by_metric(plot_data2_list[[1]], plot_data2_list[[2]])

plot_data3_list <- prepare_long_data(subsite_table, summary_gs3, metrics3)
plot_data3_list$long_subsites$metric <- factor(plot_data3_list$long_subsites$metric, levels = metrics3)
plot_data3_list$long_summary$metric <- factor(plot_data3_list$long_summary$metric, levels = metrics3)
plot_data3_df <- reorder_greenspace_by_metric(plot_data3_list[[1]], plot_data3_list[[2]])

metric_labels1 <- c(
  area_ha = "Area (ha)",
  tree_count = "Tree Abundance",
  tree_density_ha = "Tree Density (per ha)",
  richness = "Species Richness",
  hill_q1 = "Hill-Shannon Diversity (q=1)",
  pie_eve = "Evenness (Hurlbert's PIE)"
)

metric_labels2 <- c(
  area_ha = "Area (ha)",
  tree_count = "Tree Abundance",
  tree_density_ha = "Tree Density (per ha)",
  richness = "Species Richness",
  hill_q1 = "Hill-Shannon Diversity (q=1)",
  hill_q2 = "Hill-Simpson Diversity (q=2)"
)

metric_labels3 <- c(
  area_ha = "Area (ha)",
  tree_count = "Tree Abundance",
  tree_density_ha = "Tree Density (per ha)",
  richness = "Species Richness",
  shannon = "Shannon Diversity",
  pie_eve = "Evenness (Hurlbert's PIE)"
)

plot_metrics <- function(long_subsites, long_summary, metric_labels = NULL, title = NULL) {
  ggplot() +
    geom_jitter(data = long_subsites,
                aes(x = GreenSpace_ordered, y = value),
                width = 0.1, alpha = 0.3, color = "gray40", size = 1) +
    geom_point(data = long_summary,
               aes(x = GreenSpace, y = mean, color = GreenSpace),
               size = 3) +
    geom_errorbar(data = long_summary,
                  aes(x = GreenSpace, ymin = mean - se, ymax = mean + se, color = GreenSpace),
                  width = 0.2, linewidth = 1) +
    facet_wrap(~ metric, scales = "free_y", 
               labeller = if (!is.null(metric_labels)) labeller(metric = metric_labels) else "label_value") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +
    labs(title = title, x = "Green Space Type", y = "Metric Value")
}



#Plot V1:area_ha, tree_count, tree_density_ha, richness, hill_q1, and pie_eve

plot1 <- plot_metrics(plot_data1_df, plot_data1_list[[2]], metric_labels1)

print(plot1)

#Plot V2: area_m2, tree_count, tree_density_ha, hill_q1, and hill_q2

plot2 <- plot_metrics(plot_data2_df, plot_data2_list[[2]], metric_labels2)

print(plot2)

#PLot_V3: area_m2, tree_count, tree_density_ha, shannon, and pie_eve

plot3 <- plot_metrics(plot_data3_df, plot_data3_list[[2]], metric_labels3)

print(plot3)
