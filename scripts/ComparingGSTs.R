# Question 1: How does tree diversity and abundance differ across different green space types?

library("sf")
library("dplyr")
library("tidyr")
library("hillR")
library("vegan")
library("mobr")

trees <- st_read("output/trees012825_DBH.shp") %>% 
  st_drop_geometry()
landuse <- st_read("output/land012825.shp") %>% 
  st_drop_geometry()

#Separate into subsites (i.e. 01A_Commercial, 17C_Parc)

landuse$subsite <- paste(landuse$Trap, landuse$updtd_ln, sep = "_")

#Remove landuse subsites that have no trees in them 

landuse <- landuse %>%
  filter(subsite %in% unique(trees$subsite))

#Subsites that have no trees

empty_subsites <- landuse %>%
  filter(!subsite %in% unique(trees$subsite))

#Some subsites in landuse were split into two (in case where bureau and 
#commercial and industrie where lumped into commercial category),  
#so we need to combine them

#Get duplicated subsites
dupes <- landuse %>%
  dplyr::count(subsite) %>%
  filter(n > 1) %>%
  pull(subsite)

combined_rows <- list()

for (s in dupes) {
  subset_rows <- landuse %>% filter(subsite == s)
  
  combined_row <- subset_rows %>%
    summarise(
      subsite = first(subsite),
      lc_area = sum(lc_area, na.rm = TRUE),
      Trap = first(Trap),
      updtd_ln = first(updtd_ln)
    )
  
  combined_rows[[s]] <- combined_row
}

combined_df <- bind_rows(combined_rows)
landuse_no_dupes <- landuse %>% filter(!subsite %in% dupes)
landuse_cleaned <- bind_rows(landuse_no_dupes, combined_df)

saveRDS(landuse_cleaned, 'output/landuse_subsites.rds')

#Calculate variables of interest
#1. Surface area and plantable area per subsite - already have in metres, want it in ha too

#Want to also calculate PLANTABLE area, not just area

plantable_area <- readRDS("output/plantable_area.rds") %>% 
  select(subsite, plantable_area)  %>% 
  st_drop_geometry()

landuse_cleaned <- landuse_cleaned %>%
  left_join(plantable_area, by = "subsite")

area_df <- landuse_cleaned %>%
  transmute(
    subsite,
    area_m2 = lc_area,
    area_ha = lc_area / 10000, 
    plantarea_m2 = plantable_area,
    plantarea_ha = plantable_area / 10000
  )

#2. Tree abundance

tree_count_df <- group_by(trees, subsite) %>%
  summarise(tree_count = n(), .groups = "drop")

#3. Tree density
density_df <- tree_count_df %>%
  left_join(area_df, by = "subsite") %>%
  mutate(tree_density_area_ha = tree_count / area_ha,
         tree_density_plant_ha = tree_count / plantarea_ha)

#4. Diversity indices: species richness Hill numbers

species_matrix <- trees %>%
  group_by(subsite, gns_spc) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = gns_spc, values_from = count, values_fill = 0)

subsites <- species_matrix$subsite
species_data <- species_matrix %>% select(-subsite)

diversity_metrics <- tibble(
  subsite = subsites,
  
  richness = hill_taxa(species_data, q = 0),
  hill_q1 = hill_taxa(species_data, q = 1),
  hill_q2 = hill_taxa(species_data, q = 2),
  
  shannon = diversity(species_data, index = "shannon"),
  
  #hill_evenness = hill_taxa(species_data, q = 1) / hill_taxa(species_data, q = 0),
  #think about this and maybe remove 
  
  pie_eve = calc_PIE(species_data)
)

#Four subsites have a species richness of 1, but calc_PIE is returning evenness as 0.

#Join metrics into summary dataset

subsite_table <- density_df %>%
  left_join(diversity_metrics, by = "subsite")

subsite_table <- subsite_table %>%
  separate(subsite, into = c("Plot", "GreenSpace"), sep = "_", extra = "merge", remove = FALSE)

saveRDS(subsite_table, 'output/subsite_metrics.rds')

#Add rarefied abundance

rare_table <- readRDS("output/rare_combined_subsites.rds")

#Reorder so there is one row for each subsite

rare_table <- rare_table %>%
  dplyr::mutate(metric = case_when(
    metric == "Species Richness" ~ "Rare_rich",
    metric == "Hill q=1" ~ "Rare_q1",
    metric == "Hill q=2" ~ "Rare_q2",
    metric == "PIE" ~ "Rare_pie",
    TRUE ~ metric 
  )) %>%
  dplyr::select(subsite, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

#Add rarefied coverage

cov_table <- readRDS("output/rare_coverage_subsites.rds")

#Reorder so there is one row for each subsite

cov_table <- cov_table %>%
  mutate(metric = case_when(
    metric == "Species Richness" ~ "Cov_rich",
    metric == "Hill q=1" ~ "Cov_q1",
    metric == "Hill q=2" ~ "Cov_q2",
    metric == "PIE" ~ "Cov_pie",
    TRUE ~ metric 
  )) %>%
  dplyr::select(subsite, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

# --- Basal area and DBH metrics ---

#Add basal area per tree
trees <- trees %>%
  mutate(basal_area = pi * (DBH_fnl / 200)^2)  

# Median DBH per subsite
subsite_dbh <- trees %>%
  group_by(subsite, updtd_ln, Plot) %>%
  summarise(
    median_dbh = median(DBH_fnl, na.rm = TRUE),
    .groups = "drop"
  )

#Total basal area per subsite
subsite_ba <- trees %>%
  group_by(subsite, updtd_ln, Plot) %>%
  summarise(
    basal_area = sum(basal_area, na.rm = TRUE),
    .groups = "drop"
  )

#Join with area (from area_df instead of 'land', so names are consistent)
subsite_ba <- subsite_ba %>%
  left_join(area_df %>% select(subsite, area_m2, plantarea_m2), by = "subsite") %>%
  mutate(
    basal_area_plant_ha = basal_area / (plantarea_m2 / 10000),
    basal_area_ha = basal_area / (area_m2 / 10000)
  )

#Combine DBH + BA into one table
dbh_ba_table <- subsite_dbh %>%
  left_join(subsite_ba %>% 
              select(subsite, basal_area, basal_area_ha, basal_area_plant_ha),
            by = "subsite") %>%
  select(-updtd_ln, -Plot)

#Combine tables

subsite_table <- subsite_table %>%
  left_join(rare_table, join_by(subsite)) %>%
  left_join(cov_table, join_by(subsite))  %>%
  left_join(dbh_ba_table, join_by(subsite)) 
  

saveRDS(subsite_table, 'output/diversitymetrics_table_rare.rds')

