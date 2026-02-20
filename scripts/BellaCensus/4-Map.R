
# Packages ----------------------------------------------------------------
source('scripts/0-packages.R')


# Data --------------------------------------------------------------------

# your plots
plots <- read_sf('output/Plot_Deprivation.gpkg')

# download data for the island of Montreal for the background of the map 
# choose the coordinates of the region you are interested in (got these from Google Maps)
bb <- c(xmin = -74.0788,
        ymin = 45.3414,
        xmax = -73.3894,
        ymax = 45.7224)

# Use the coordinates you extracted to download all the things called islands within that range
# Download island boundary in bbox
mtl <- opq(bb) %>%
  add_osm_feature(key = 'place', value = 'island') %>%
  osmdata_sf() # returns an object with points, lines, polygons, and multipolygons
# Grab multipolygons (large islands)
multipolys <- mtl$osm_multipolygons
# Grab polygons (small islands)
polys <- mtl$osm_polygons
polys <- st_cast(polys, "MULTIPOLYGON")
# Combine geometries and cast as sf
allpolys <- st_as_sf(st_union(polys, multipolys))

# Now use the coordinates you extracted to download all the things called water within that range
water <- opq(bb) %>%
  add_osm_feature(key = 'natural', value = 'water') %>%
  osmdata_sf()
# We only want multipolygons (aka large rivers)
mpols <- water$osm_multipolygons
mpols <- st_cast(mpols, "MULTIPOLYGON")
mpols <- st_as_sf(st_make_valid(mpols))


# Montreal plot -----------------------------------------------------------

# make the coordinates of your plot a little bit larger than the exact boundaries of all the polygons you extracted
# (you don't want the edge of your plot to be touching the border of the island of Montreal)
bbi <- st_bbox(st_buffer(allpolys, 2.5))

cens <- st_centroid(plots)

map <- ggplot() +
  # add island layer to figure
  geom_sf(fill = '#ceb99b', data = allpolys) + 
  # add water layer to figure
  geom_sf(fill = '#99acc3', data = mpols) + 
  # add plot layer to figure
  geom_sf(aes(colour = sitvul_score_mean), size = 3.5, data = cens) +
  # explicitly state where you want boundaries of your figure using line above
  coord_sf(xlim = c(bbi['xmin'], bbi['xmax']),
           ylim = c(bbi['ymin'], bbi['ymax'])) +
  scale_colour_viridis_c(option = "D") +  
  labs(colour = "Mean Situational Vulnerability") + 
  theme(panel.border = element_rect(linewidth = 1, fill = NA),
        panel.background = element_rect(fill = '#ddc48d'),
        panel.grid = element_line(color = '#73776F', linewidth = 0.2),
        axis.text = element_text(size = 11, color = 'black'),
        axis.title = element_blank(), 
        plot.background = element_rect(fill = NA, colour = NA),
        legend.position = 'top')
map

ggsave('graphics/SituationalVulnerabilityMap.png', map, height = 12, width = 12, units = 'in')
