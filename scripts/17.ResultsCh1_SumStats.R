#Urban Forest Inventory Summary Results

library(sf)
library(dplyr)
library(ggplot2)
library(forcats)
library(patchwork)


trees <- st_read("output/trees012825_DBH.shp") %>% 
  st_drop_geometry()

land <- readRDS("output/plantable_area.rds")

#Number of trees, species, genus total

n_distinct(trees)
n_distinct(trees$genus)
n_distinct(trees$gns_spc)

#Number of trees, species, genus per GST

trees %>%
  group_by(updtd_ln) %>%
  summarise(
    n_trees   = n(),                        
    species   = list(unique(gns_spc)),      
    genera    = list(unique(genus))         
  )

private_gst <- c("Commercial", "Institutional", "Residential", "Vacant Lot")
public_gst  <- c("Park", "Public Right-Of-Way")

#Percent of plantable and subsite land that each GST has

gst_summary <- land %>%
  group_by(updtd_ln) %>%
  summarise(
    n_subsites      = n(),
    avg_subsite_area_ha   = mean(subsite_area, na.rm = TRUE) / 10000,
    avg_plantable_area_ha = mean(plantable_area, na.rm = TRUE) / 10000,
    total_area_ha         = sum(subsite_area, na.rm = TRUE) / 10000,
    total_plantable_ha    = sum(plantable_area, na.rm = TRUE) / 10000,
    .groups = "drop"
  ) %>%
  mutate(
    percent_total_area     = 100 * total_area_ha / sum(total_area_ha, na.rm = TRUE),
    percent_plantable_area = 100 * total_plantable_ha / sum(total_plantable_ha, na.rm = TRUE)
  )

gst_summary

group_summary <- gst_summary %>%
  mutate(group = ifelse(updtd_ln %in% public_gst, "Public", "Private")) %>%
  group_by(group) %>%
  summarise(
    n_subsites           = sum(n_subsites),
    avg_subsite_area_ha     = mean(avg_subsite_area_ha, na.rm = TRUE),
    avg_plantable_area_ha   = mean(avg_plantable_area_ha, na.rm = TRUE),
    total_area_ha           = sum(total_area_ha),
    total_plantable_ha      = sum(total_plantable_ha),
    .groups = "drop"
  ) %>%
  mutate(
    percent_total_area     = 100 * total_area_ha / sum(total_area_ha),
    percent_plantable_area = 100 * total_plantable_ha / sum(total_plantable_ha)
  )

group_summary

#Percent hedge and numbner of hedge per GST

hedge_counts <- trees %>%
  group_by(updtd_ln) %>%
  summarise(
    total_trees = n(),
    hedge_trees = sum(Hedg_YN == "Y", na.rm = TRUE)
  ) %>%
  mutate(
    prop_hedge = hedge_trees / total_trees * 100   # % hedges within GST
  )

hedge_counts

residential_hedges <- hedge_counts %>%
  filter(updtd_ln == "Residential") %>%
  select(updtd_ln, total_trees, hedge_trees, prop_hedge)


# Species lists
species_private <- trees %>%
  filter(updtd_ln %in% private_gst) %>%
  pull(gns_spc) %>%
  unique()

species_public <- trees %>%
  filter(updtd_ln %in% public_gst) %>%
  pull(gns_spc) %>%
  unique()

# Unique contribution from private GSTs
unique_private <- setdiff(species_private, species_public)

# Numbers
n_private_unique <- length(unique_private)
n_total_species  <- length(unique(c(species_private, species_public)))
n_public_species <- length(species_public)

# Proportion captured by public inventory
prop_public <- n_public_species / n_total_species * 100

# Unique contributions from each private GST
unique_private_contrib <- trees %>%
  filter(updtd_ln %in% private_gst) %>%
  group_by(updtd_ln) %>%
  summarise(
    unique_species = list(setdiff(unique(gns_spc), species_public)),
    n_unique = lengths(unique_species),
    .groups = "drop"
  )

unique_private_contrib

# Species by GST
species_by_gst <- trees %>%
  group_by(updtd_ln) %>%
  summarise(species = list(unique(gns_spc)), .groups = "drop")

# All species in residential
species_residential <- species_by_gst$species[species_by_gst$updtd_ln == "Residential"][[1]]

# All species in every other GST
species_others <- species_by_gst %>%
  filter(updtd_ln != "Residential") %>%
  pull(species) %>%
  unlist() %>%
  unique()

# Exclusive to residential
species_exclusive_res <- setdiff(species_residential, species_others)

species_exclusive_res
length(species_exclusive_res)


#Most common species (top 10) overall and in each GST
total_trees <- nrow(trees)

top10_all <- trees %>%
  count(gns_spc, name = "n_trees") %>%
  mutate(percent_total = 100 * n_trees / total_trees) %>%
  arrange(desc(n_trees)) %>%
  slice_head(n = 10)

top10_allgenus <- trees %>%
  count(genus, name = "n_trees") %>%
  mutate(percent_total = 100 * n_trees / total_trees) %>%
  arrange(desc(n_trees)) %>%
  slice_head(n = 10)

top10_by_gst <- trees %>%
  group_by(updtd_ln, gns_spc) %>%
  summarise(n_trees = n(), .groups = "drop") %>%
  group_by(updtd_ln) %>%
  mutate(
    total_gst_trees = sum(n_trees),
    percent_within_gst = 100 * n_trees / total_gst_trees
  ) %>%
  arrange(desc(n_trees), .by_group = TRUE) %>%
  slice_head(n = 10)

top3_by_gst <- trees %>%
  group_by(updtd_ln, gns_spc) %>%
  summarise(n_trees = n(), .groups = "drop") %>%
  group_by(updtd_ln) %>%
  mutate(
    total_gst_trees = sum(n_trees),
    percent_within_gst = 100 * n_trees / total_gst_trees
  ) %>%
  arrange(desc(n_trees), .by_group = TRUE) %>%
  slice_head(n = 3)


#DBH summary overall + GST
dbh_summary <- trees %>%
  summarise(
    min_dbh    = min(DBH_fnl, na.rm = TRUE),
    median_dbh = median(DBH_fnl, na.rm = TRUE),
    max_dbh    = max(DBH_fnl, na.rm = TRUE)
  )

dbh_by_gst <- trees %>%
  group_by(updtd_ln) %>%
  summarise(
    median_dbh = median(DBH_fnl, na.rm = TRUE),
    .groups = "drop"
  )

#Total basal area per hecatre for each GST

subsite_table <- readRDS("output/diversitymetrics_table_rare.rds")

ba_per_ha_by_gst <- subsite_table %>%
  mutate(
    subsite_area_ha = subsite_area / 10000,
    # absolute basal area (m²) per subsite
    total_ba_all_m2    = basal_area_ha * plot_area_ha,
    total_ba_planted_m2 = basal_area_plant_ha * plot_area_ha
  ) %>%
  group_by(updtd_ln) %>%
  summarise(
    total_ba_all_m2     = sum(total_ba_all_m2, na.rm = TRUE),
    total_ba_planted_m2 = sum(total_ba_planted_m2, na.rm = TRUE),
    total_area_ha       = sum(plot_area_ha, na.rm = TRUE),
    ba_all_per_ha       = total_ba_all_m2 / total_area_ha,
    ba_planted_per_ha   = total_ba_planted_m2 / total_area_ha,
    .groups = "drop"
  )


#Plot of most common species private vs public 

# Abundance (counts)

trees <- trees %>%
  mutate(Ownership = ifelse(updtd_ln %in% private_gst, "Private",
                            ifelse(updtd_ln %in% public_gst, "Public", NA)),
         basal_area = pi * (DBH_fnl / 200)^2)

species_summary_abund <- trees %>%
  filter(!is.na(Ownership)) %>%
  group_by(Ownership, gns_spc) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Ownership) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  slice_max(order_by = Percent, n = 10) %>%
  ungroup()

private_abund <- species_summary_abund %>%
  filter(Ownership == "Private") %>%
  arrange(desc(Percent)) %>%
  mutate(gns_spc_ordered = factor(gns_spc, levels = gns_spc))

p_private_abund <- ggplot(private_abund, aes(x = gns_spc_ordered, y = Percent)) +
  geom_col(fill = "grey") +
  scale_x_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
  labs(x = "Species", y = "Relative abundance (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("A: Private (Stem abundance)")

public_abund <- species_summary_abund %>%
  filter(Ownership == "Public") %>%
  arrange(desc(Percent)) %>%
  mutate(gns_spc_ordered = factor(gns_spc, levels = gns_spc))

p_public_abund <- ggplot(public_abund, aes(x = gns_spc_ordered, y = Percent)) +
  geom_col(fill = "grey") +
  scale_x_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
  labs(x = "Species", y = "Relative abundance (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("B: Public (Stem abundance)")


# Relative basal area

species_summary_basal <- trees %>%
  filter(!is.na(Ownership)) %>%
  group_by(Ownership, gns_spc) %>%
  summarise(BasalArea = sum(basal_area, na.rm = TRUE), .groups = "drop") %>%
  group_by(Ownership) %>%
  mutate(Percent = BasalArea / sum(BasalArea) * 100) %>%
  slice_max(order_by = Percent, n = 10) %>%
  ungroup()

private_basal <- species_summary_basal %>%
  filter(Ownership == "Private") %>%
  arrange(desc(Percent)) %>%
  mutate(gns_spc_ordered = factor(gns_spc, levels = gns_spc))

p_private_basal <- ggplot(private_basal, aes(x = gns_spc_ordered, y = Percent)) +
  geom_col(fill = "grey") +
  scale_x_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
  labs(x = "Species", y = "Relative importance (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("C: Private (Basal area)")

public_basal <- species_summary_basal %>%
  filter(Ownership == "Public") %>%
  arrange(desc(Percent)) %>%
  mutate(gns_spc_ordered = factor(gns_spc, levels = gns_spc))

p_public_basal <- ggplot(public_basal, aes(x = gns_spc_ordered, y = Percent)) +
  geom_col(fill = "grey") +
  scale_x_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
  labs(x = "Species", y = "Relative importance (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("D: Public (Basal area)")

(p_private_abund + p_public_abund) /
  (p_private_basal + p_public_basal)


#Species composition

species_richness <- trees %>%
  filter(!is.na(Ownership)) %>%
  group_by(Ownership) %>%
  summarise(n_species = n_distinct(gns_spc), .groups = "drop")

# total richness across all ownership types
total_species <- n_distinct(trees$gns_spc)

# proportion of all species per ownership
species_richness <- species_richness %>%
  mutate(proportion = n_species / total_species * 100)

species_richness

#Same but for GST

species_richness_gst <- trees %>%
  filter(!is.na(updtd_ln)) %>%
  group_by(updtd_ln) %>%
  summarise(n_species = n_distinct(gns_spc), .groups = "drop")

total_species <- n_distinct(trees$gns_spc)

species_richness_gst <- species_richness_gst %>%
  mutate(proportion = n_species / total_species * 100) %>%
  arrange(desc(proportion))

species_richness_gst


###PUBLIC TREE CSV

public <- read.csv("input/large/arbres-publics.csv")

library(dplyr)

public %>%
  summarize(
    percent_thuja = mean(grepl("Thuja occidentalis", Essence_latin, ignore.case = TRUE)) * 100
  )

public %>%
  summarize(
    percent_lilac = mean(grepl("Syringa", Essence_latin, ignore.case = TRUE)) * 100
  )

public %>%
  summarize(
    percent_acer = mean(grepl("Acer platanoides", Essence_latin, ignore.case = TRUE)) * 100
  )


