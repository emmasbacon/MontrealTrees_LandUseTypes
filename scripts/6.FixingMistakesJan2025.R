#Just edited DBH column 01/21/25


trees <- st_read("output/treedataupdatedjun13.shp")

landuse <- st_read("output/plots_landcover.shp")


# GSTs  -------------------------------------------------------------------

#First, remove plots 19, 20, and 23 due to incomplete data set. Doing this also removes agricole from GSTs because that is only present in those three

trees <- trees %>% filter(!Plot %in% c("19B", "20C", "23A"))
landuse <- landuse %>% filter(!Trap %in% c("19B", "20C", "23A"))

#For graphs, rename GST

landuse <- landuse %>%
  mutate(updated_lnd = recode(land_cover,
                              "commerciale" = "Commercial",
                              "résidentielle" = "Residential",
                              "institutionnelle" = "Institutional",
                              "parc" = "Park",
                              "terrain vacant" = "Vacant Lot",
                              "utilité publique" = "Public Right-Of-Way"
  ))

trees <- trees %>%
  mutate(updated_lnd = recode(lnd_cvr,
                              "commerciale" = "Commercial",
                              "résidentielle" = "Residential",
                              "institutionnelle" = "Institutional",
                              "parc" = "Park",
                              "terrain vacant" = "Vacant Lot",
                              "utilité publique" = "Public Right-Of-Way"
  ))


#Some trees are +175 DBH and that is a mistake (goes from 175 to 37317. No DBH_UQAM or DBH_Ville is available, so change all DBH for DBH_fnl over 175 to NA

trees <- trees %>%
  mutate(DBH_fnl = ifelse(DBH_fnl > 175, NA, DBH_fnl))

#Remove Measure or Estimated for trees with no DBH

trees <- trees %>%
  mutate(
    M_M__E_ = if_else(is.na(DBH_fnl), NA, M_M__E_)
  )

#Remove unnecessary columns

trees <- trees %>% select(-DBH_UQA, -DBH_VMT, -H_S_L_W, -Hdg_NmS, -Comment, -Citizen)


write_sf(trees, 'output/land012825.shp')
write_sf(trees, 'output/trees012125.shp')