
# Packages ----------------------------------------------------------------
source('scripts/BellaCensus/0-packages.R')

# Data --------------------------------------------------------------------

df <- read.csv('input/Inventoried_Trees2023_WORKING.csv')

# convert to spatial object 

df_spat <- df %>% 
  # drop any rows that do not have a geotag
  drop_na(c(Latitude, Longitude)) %>% 
  # use sf package to make this into a projected spatial object
  st_as_sf(coords = c("Latitude", "Longitude"), crs = 4326) # 4326 = WGS84

#mapview use to check * Emma 

# turn groups of points into polygons
df_poly <- df_spat %>%
  group_by(Plot) %>%
  summarise() %>%
  st_convex_hull()

# create buffer surrounding each plot to extract census data of interest
# transform to a projection that is in metres so that our buffer units make sense 
df_poly_trans <- st_transform(df_poly, crs = 3347)
buffers <- st_buffer(df_poly_trans, 100)


# Save --------------------------------------------------------------------

saveRDS(df_spat, 'output/TreesSpatial.rds')
saveRDS(df_poly_trans, 'output/PlotsSpatial.rds')
saveRDS(buffers, 'output/PlotBuffers.rds')
