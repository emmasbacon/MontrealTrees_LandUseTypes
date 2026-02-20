#Built Area Data from QGIS/OSM

library(sf)
library(dplyr)

builtarea <- st_read("input/BuiltArea_R.shp")

colnames(builtarea)

builtarea <- builtarea %>% dplyr::select(-calque, -projection, -fclass, -Adress, -bridge, -tunnel)

# Calculate plantable area, proportion, and percentage
builtarea <- builtarea %>%
  mutate(plantable_area = area_2 - built_area,  # Calculate plantable land
         built_proportion = built_area / area_2,  # Proportion of built area
         plantable_proportion = plantable_area / area_2,  # Proportion of plantable land
         built_percentage = built_proportion * 100,  # Convert to percentage
         plantable_percentage = plantable_proportion * 100)  # Convert to percentage

table_built <- builtarea %>%
  dplyr::select(Trap, plantable_area, built_area, built_proportion, plantable_proportion, built_percentage, plantable_percentage)

write.csv(table_built, "output/builtareapercent.csv", row.names = FALSE)
write_sf(table_built, "output/builtarea.shp")

#Want to determine plantable area per subsite, instead of using area that includes built area

landuse_subsites <- st_read("output/landuse_cleaned_area.shp")
table_built <- st_transform(table_built, st_crs(landuse_subsites))

#Split overlapping built polygons into non-overlapping pieces
built_split <- st_union(table_built) %>% 
  st_cast("POLYGON") %>% 
  st_sf()

#Intersect subsites with the split built polygons (non-overlapping pieces)
built_in_subsites <- st_intersection(landuse_subsites, built_split) %>%
  mutate(piece_area = st_area(geometry)) #calculate area

#Summarise total built area per subsite (drop geometry for join)
built_area_by_subsite <- built_in_subsites %>%
  st_drop_geometry() %>%
  group_by(subsite) %>%
  summarise(total_built = sum(piece_area)) %>%
  ungroup()

#Add total subsite area and join built area back in
subsites_with_areas <- landuse_subsites %>%
  mutate(subsite_area = as.numeric(st_area(geometry))) %>%
  left_join(built_area_by_subsite, by = "subsite") %>%
  mutate(
    total_built = ifelse(is.na(total_built), 0, as.numeric(total_built)),
    plantable_area = subsite_area - total_built
  )

saveRDS(subsites_with_areas, "output/plantable_area.rds")
