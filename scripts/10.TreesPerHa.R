#Trees per ha for each GST

library(sf)
library(dplyr)

trees <- st_read("output/trees012825.shp")

landuse <- st_read("output/land012825.shp")


tree_counts <- trees %>%
  group_by(Plot, updtd_l) %>%
  summarise(tree_count = n(), .groups = 'drop')

#Trees and landuse are not in the same crs, fix

st_crs(trees)
st_crs(landuse)

trees <- st_transform(trees, st_crs(landuse))

tree_counts_sf <- trees %>%
  group_by(Plot, updtd_l) %>%
  summarise(tree_count = n(), .groups = 'drop') %>%
  st_as_sf() %>%
  st_transform(st_crs(landuse))

tree_density_data <- st_join(tree_counts_sf, landuse)

#Get trees per ha

tree_density_data <- tree_density_data %>%
  mutate(tree_density_per_hectare = (tree_count / lc_area) * 10000)

#Standard error for each plot

tree_density_se <- tree_density_data %>%
  group_by(Plot, updtd_l.x) %>%
  summarise(
    mean_density = mean(tree_density_per_hectare),
    se_density = sd(tree_density_per_hectare) / sqrt(n()),
    .groups = 'drop'
  )

#For each GST

tree_density_se <- tree_density_data %>%
  group_by(updtd_l.x) %>%  # Group only by land use type (GST)
  summarise(
    mean_density = mean(tree_density_per_hectare, na.rm = TRUE),  # Mean for each GST
    se_density = sd(tree_density_per_hectare, na.rm = TRUE) / sqrt(n()),  # SE for each GST
    .groups = 'drop'
  )

#Graph

ggplot(tree_density_se, aes(x = updtd_l.x, y = mean_density, color = updtd_l.x)) +
  geom_point(size = 3) +  # All dots will have the same shape
  geom_errorbar(
    aes(ymin = mean_density - se_density, ymax = mean_density + se_density),
    width = 0.2  # Error bar width
  ) +
  labs(
    title = "Tree Density by GST Type",
    x = "Land Use Type (GST)",
    y = "Tree Density (per hectare)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Log scaled becasue residential is so much higher

ggplot(tree_density_se, aes(x = updtd_l.x, y = mean_density, color = updtd_l.x)) +
  geom_point(size = 3) +  # All dots will have the same shape
  geom_errorbar(
    aes(ymin = mean_density - se_density, ymax = mean_density + se_density),
    width = 0.2  # Error bar width
  ) +
  scale_y_log10() +  # Apply log scale to the y-axis
  labs(
    title = "Tree Density by GST Type (Log Scale)",
    x = "Land Use Type (GST)",
    y = "Tree Density (per hectare, log scale)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
        