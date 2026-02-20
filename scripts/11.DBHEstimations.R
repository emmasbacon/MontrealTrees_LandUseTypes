##Sep 4th 2025

library("sf")
library("dplyr")
library("stringr")

trees <- st_read("output/trees012825.shp")

DBH_summary <- trees %>%
  summarize(
    DBH_missing = sum(is.na(DBH_fnl)),
    DBH_present = sum(!is.na(DBH_fnl))
  ) %>%
  mutate(
    percent_identified = 100 * DBH_present / (DBH_present + DBH_missing)
  )

#404 trees missing DBH

species_missing_dbh <- trees %>%
  filter(is.na(DBH_fnl)) %>%
  group_by(gns_spc) %>%
  summarize(missing_DBH_count = n()) %>%
  arrange(desc(missing_DBH_count))

#63 species missing DBH

trees_missing_dbh <- trees %>%
  filter(is.na(DBH_fnl))

#Change M_M__E__ value to E for those missing DBH, since we are estimating them

trees <- trees %>%
  mutate(M_M__E_ = ifelse(
    is.na(DBH_fnl) | M_M__E_ %in% c(2, 59),
    "E",
    M_M__E_
  ))

# Plot08A -----------------------------------------------------------------

n_missing <- trees %>%
  filter(Plot == "08A", is.na(DBH_fnl)) %>%
  nrow()

#In Plot 08A, there are 158 trees missing DBH, but they are located along a railroad where measuring is impossible. 
#So, we walked the length of the railroad (where accessible) and created an average DBH for species within
#this area, ideally 5+ estimated individuals to get our average and then apply it to the trees in Plot 8A missing DBH
#In some cases, this was not possible, so we measured as many individuals as we saw and created an average using that
#For four species, we could not find trees within the railroad that had previously been recorded due to access issues to certain areas of the railroad
#In these cases, we used the overall process (GST, overall average) to estimate DBH
#FOUR SPECIES: Acer rubrum, Quercus robur, Sorbus aucuparia, Ulmus pumila

#Dataset with calculated averages for Plot 8A

trees_est <- read.csv("input/Plot8_EstDBH_Aug25.csv")

#Convert multistemmed trees per Magarik et. al 2020

trees_est <- trees_est %>%
  rowwise() %>%
  mutate(
    DBH_numeric = ifelse(
      !is.na(EstimatedDBH_Aug25) & str_detect(EstimatedDBH_Aug25, "/"),  # multi-stemmed check
      sqrt(sum(as.numeric(str_split(EstimatedDBH_Aug25, "/", simplify = TRUE))^2)),
      as.numeric(EstimatedDBH_Aug25)  # single stem
    )
  ) %>%
  ungroup()

#Get average DBH for each species 

dbh_avg_08A <- trees_est %>%
  group_by(Species) %>%
  summarise(mean_DBH = mean(DBH_numeric, na.rm = TRUE))


#Add this average as the DBH_fnl for trees missing DBH in Plot8A

trees <- trees %>%
  left_join(dbh_avg_08A, by = c("gns_spc" = "Species")) %>%  # match columns correctly
  mutate(
    DBH_fnl = ifelse(Plot == "08A" & is.na(DBH_fnl), mean_DBH, DBH_fnl)
  ) %>%
  select(-mean_DBH)


# Estimating GST-Species Avg ----------------------------------------------

DBH_summary2 <- trees %>%
  summarize(
    DBH_missing = sum(is.na(DBH_fnl)),
    DBH_present = sum(!is.na(DBH_fnl))
  )
#261 individuals still missing DBH, from across all Plots

#Create averages only for species with 5 or more occurences

dbh_avg_gst <- trees %>%
  group_by(gns_spc, updtd_ln) %>%
  summarise(
    n = n(),
    mean_DBH = mean(DBH_fnl, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  st_drop_geometry()

#Now, replace NA in DBH column with the site specific species average

trees <- trees %>%
  left_join(dbh_avg_gst, by = c("gns_spc", "updtd_ln")) %>%
  mutate(
    DBH_fnl = ifelse(is.na(DBH_fnl) & !is.na(mean_DBH), mean_DBH, DBH_fnl)
  ) %>%
  select(-mean_DBH)

# Estimating Overall Species Avg ------------------------------------------

DBH_summary3 <- trees %>%
  summarize(
    DBH_missing = sum(is.na(DBH_fnl)),
    DBH_present = sum(!is.na(DBH_fnl))
  )

#There are still 24 trees that are NA in DBH column, because they do not have 5 individuals or more in a specific GST. 
#In these cases, we will simply estimate DBH by using the average of all individuals from that species

species_low_count_overall <- trees %>%
  group_by(gns_spc) %>%
  summarise(
    n_total = n(),
    n_na = sum(is.na(DBH_fnl)),
    .groups = "drop"
  ) %>%
  filter(n_total < 5 & n_na > 0)

#5 individuals with no DBH were in a species that had less than 5 occurences overall. In these cases, we still decided to use the average of all individuals
#5 species with less than 5 occurences where DBH was missing for one individual:
#Ficus carica,Carya glabra, Rosa rubiginosa, Salix eleagnos, Spiraea japonica

#Average dbh per species across all plots

dbh_avg_all <- trees %>%
  group_by(gns_spc) %>%
  summarise(mean_DBH_species = mean(DBH_fnl, na.rm = TRUE),
    .groups = "drop") %>%
  st_drop_geometry()

#Replace in trees dataset for individuals who have NA in DBH column still 

trees <- trees %>%
  left_join(dbh_avg_all, by = "gns_spc") %>%
  mutate(
    DBH_fnl = ifelse(is.na(DBH_fnl), mean_DBH_species, DBH_fnl)
  ) %>%
  select(-mean_DBH_species, -n)

#Finally, there is one individual where it is the only individual, and has no measured DBH. 
#The mean averages for the other genus Spiraea individuals was 3.1 (Spiraea trilobata) and 7 (Spiraea nipponica)
#We decided to give Spiraea japonica an estimated DBH of 3 to be conservative and due to the similar function

trees <- trees %>%
  mutate(
    DBH_fnl = ifelse(gns_spc == "Spiraea japonica" & is.na(DBH_fnl), 3, DBH_fnl)
  )

#DBH has been added to the dataset

write_sf(trees, "output/trees012825_dbh.shp")


