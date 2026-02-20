
# Packages ----------------------------------------------------------------
source('scripts/0-packages.R')


# Data --------------------------------------------------------------------

# cimd data 
cimd_da <- readRDS('output/BellaCensus/CIMD_DA.rds')

# site buffers from ArcGIS online (my FieldMaps for the plots)
# gpkg conserves the spatial aspects 
buffers <- read_sf('input/Trees_inventory_2023_WFL1_buffer200.gpkg')
buffers_trans <- st_transform(buffers, crs = 3347)

# ensure that projection of cimd data is the same as your buffers 
cimd_da_spat <- cimd_da %>% 
  st_as_sf() %>% 
  st_transform(crs = st_crs(buffers_trans))


# Extract Data ------------------------------------------------------------
# extract all the cimd data that overlaps the buffers 
cimd_buffers <- st_intersection(cimd_da_spat, buffers_trans) %>% 
  mutate(area_m2 = drop_units(st_area(geometry)))

# multiple buffers per DA - combine the geometries and calculate average of each value based on spatial weighting 
cimd_plots <- cimd_buffers %>%
  group_by(Trap) %>%   
  summarize(DAcount = n(),
            DA_pop_sum = sum(Dissemination.area..DA..Population),
            resinst_score_mean = weighted.mean(Residential.instability.Scores, area_m2),
            resint_quint_mean = weighted.mean(Residential.instability.Quintiles, area_m2),
            ecodep_score_mean = weighted.mean(Economic.dependency.Scores, area_m2),
            ecodep_quint_mean = weighted.mean(Economic.dependency.Quintiles, area_m2),
            ethcomp_score_mean = weighted.mean(Ethno.cultural.composition.Scores, area_m2),
            ethcomp_quint_mean = weighted.mean(Ethno.cultural.composition.Quintiles, area_m2),
            sitvul_score_mean = weighted.mean(Situational.vulnerability.Scores, area_m2),
            sitvul_quin_mean = weighted.mean(Situational.vulnerability.Quintiles, area_m2),
            geometry = st_union(geometry)) %>%
  distinct(Trap, .keep_all = TRUE)

# IGNORE currently there is a point that is not assigned a plot, going to remove it so it doesn't mess with mapping - to be discussed
cimd_plots_filt <- cimd_plots %>% 
  filter(Plot != "")


# save finished dataset (save one spatial version and one non spatial)
write_sf(cimd_plots, 'output/Plot_Deprivation.gpkg') # GPKG is a type of file that can be used in most spatial software, including ArcGIS
write.csv(cimd_plots %>% st_set_geometry(NULL), 'output/Plot_Deprivation.csv')
