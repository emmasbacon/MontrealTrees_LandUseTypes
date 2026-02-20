#CEF Graphs

library(dplyr)
library(sf)
library(tidyr)
library(vegan)
library(ggplot2)
library(tibble)

trees <- st_read("output/trees012825.shp")
landuse <- st_read("output/land012825.shp")

trees <- st_drop_geometry(trees)
landuse <- st_drop_geometry(landuse)

# Question 1: GST ---------------------------------------------------------

#create combined_plotdat but for GST

gst_area <- landuse %>%
  group_by(updtd_ln) %>%
  summarise(total_area = sum(lc_area, na.rm = TRUE))

gst_area <- gst_area %>%
  rename(updtd_l = updtd_ln)

gst_trees <- trees %>%
  group_by(updtd_l) %>%
  summarise(tree_count = n())

gst_richness <- trees %>%
  group_by(updtd_l) %>%
  summarise(species_richness = n_distinct(gns_spc))

#Shannon
species_counts <- trees %>%
  group_by(updtd_l, gns_spc) %>%
  summarise(count = n(), .groups = "drop")

shannon_matrix <- species_counts %>%
  pivot_wider(names_from = gns_spc, values_from = count, values_fill = 0)

shannon_values <- shannon_matrix %>%
  column_to_rownames("updtd_l") %>%
  diversity(index = "shannon") %>%
  enframe(name = "updtd_l", value = "shannon_diversity")

gst_combined <- gst_area %>%
  left_join(gst_trees, by = "updtd_l") %>%
  left_join(gst_richness, by = "updtd_l") %>%
  left_join(shannon_values, by = "updtd_l")

#Pielou Eveness
gst_combined <- gst_combined %>%
  mutate(evenness = ifelse(species_richness > 1,
                           shannon_diversity / log(species_richness),
                           NA))

gst_combined <- gst_combined %>%
  mutate(
    tree_density_m2 = tree_count / total_area,
    richness_density_m2 = species_richness / total_area,
    
    #trees per hectare (10,000 m²)
    tree_density_ha = tree_density_m2 * 10000,
    richness_density_ha = richness_density_m2 * 10000
  )

#Compare Shannon with Hill Shannon

library(hillR)

species_matrix <- trees %>%
  group_by(updtd_l, gns_spc) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = gns_spc, values_from = count, values_fill = 0) %>%
  column_to_rownames("updtd_l")

shannon_q1 <- hillR::hill_taxa(species_matrix, q = 1)

shannon_q1_df <- data.frame(
  updtd_l = names(shannon_q1),
  shannon_hill = as.numeric(shannon_q1)
)

gst_combined <- gst_combined %>%
  left_join(shannon_q1_df, by = "updtd_l")

#Graphs

gst_combined$updtd_l <- factor(
  gst_combined$updtd_l,
  levels = c("Commercial", "Institutional", "Park", "Public Right-Of-Way", "Residential", "Vacant Lot"),
  labels = c("Commerciale", "Institutionnelle", "Parc", "Utilité publique", "Résidentiels", "Terrains vacants")
)

#1 Area per GST 

ggplot(gst_combined, aes(x = reorder(updtd_l, total_area), y = total_area, color = updtd_l)) +
  geom_point(size = 5) +
  labs(
    x = NULL,
    y = "Surface (m²)",
    color = "Type d'espaces verts"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_blank()
  )

#2 Number of trees per GST

ggplot(gst_combined, aes(x = reorder(updtd_l, tree_count), y = tree_count, color = updtd_l)) +
  geom_point(size = 5) +
  labs(
    x = NULL,
    y = "Nombre d'arbres",
    color = "Type d'espaces verts"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_blank()
  )

#3 Species richness per GST

ggplot(gst_combined, aes(x = reorder(updtd_l, species_richness), y = species_richness, color = updtd_l)) +
  geom_point(size = 5) +
  labs(
    x = NULL,
    y = "Richesse des espèces",
    color = "Type d'espaces verts"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_blank()
  )

#4 Rarefaction species richness per GST 

#5 Shannon Diversity per GST

ggplot(gst_combined, aes(x = reorder(updtd_l, shannon_hill), 
                         y = shannon_hill, color = updtd_l)) +
  geom_point(size = 5) +
  labs(
    x = NULL,
    y = "Indice de Shannon",
    color = "Type d'espaces verts"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_blank()
  )

#Just realized I want standard deviation for graph 1 2 and 3

plot_level_data <- trees %>%
  group_by(Plot, updtd_l) %>%
  summarise(
    tree_count = n(),
    species_richness = n_distinct(gns_spc),
    .groups = "drop"
  )

landuse <- landuse %>%
  rename(Plot = Trap) %>%
  select(-updtd_l) 

landuse <- landuse %>%
  rename(updtd_l = updtd_ln)

plot_level_data <- plot_level_data %>%
  left_join(landuse %>% select(Plot, updtd_l, lc_area), by = c("Plot", "updtd_l"))

#Note there are less rows in plot_level_data than landuse 
#because some gst did not have trees in some plots

summary_data <- plot_level_data %>%
  group_by(updtd_l) %>%
  summarise(
    mean_area = mean(lc_area, na.rm = TRUE),
    sd_area = sd(lc_area, na.rm = TRUE),
    mean_richness = mean(species_richness, na.rm = TRUE),
    sd_richness = sd(species_richness, na.rm = TRUE),
    mean_tree_count = mean(tree_count, na.rm = TRUE),
    sd_tree_count = sd(tree_count, na.rm = TRUE)
  )

summary_data$updtd_l <- factor(
  summary_data$updtd_l,
  levels = c("Commercial", "Institutional", "Park", "Public Right-Of-Way", "Residential", "Vacant Lot"),
  labels = c("Commerciale", "Institutionnelle", "Parc", "Utilité publique", "Résidentiels", "Terrains vacants")
)

#Mean area per plot for each GST

ggplot(summary_data, aes(x = reorder(updtd_l, mean_area), y = mean_area, color = updtd_l)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = mean_area - sd_area, ymax = mean_area + sd_area), width = 0.2) +
  labs(
    y = "Surface moyenne occupée par placette (m²)",
    color = "Type d'espaces verts"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_blank()
  )

#Mean # of trees for each plot 

ggplot(summary_data, aes(x = reorder(updtd_l, mean_tree_count), y = mean_tree_count, color = updtd_l)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = mean_tree_count - sd_tree_count, ymax = mean_tree_count + sd_tree_count), width = 0.2) +
  labs(
    y = "Nombre d'arbres moyenne",
    color = "Type d'espaces verts"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_blank()
  )

ggplot(summary_data, aes(x = reorder(updtd_l, mean_richness), y = mean_richness, color = updtd_l)) +
  geom_point(size = 7) +
  geom_errorbar(aes(ymin = mean_richness - sd_richness, ymax = mean_richness + sd_richness), width = 0.2) +
  labs(
    y = "Richesse moyenne",
    color = "Type d'espaces verts"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.title = element_blank()
  )

#Question 2, see Models 18

#See tree proportion based on location - built density 

#Population density and built area are heavily correlated (0.72), so it areas 
#with higher population we see higher built area. we were interested in whether 
#different types of green spaces, or private and public trees matter more in certain areas

model_built




