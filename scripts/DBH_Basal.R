#DBH Basal Area and %Area Qs

library("sf")
library("dplyr")
library("tidyr")

#First question - what percent of each GST is plantable area? 
#Is there more plantable area in some GSTs than others?

land <- readRDS("output/plantable_area.rds")

land <- land %>%
  mutate(plantable_pct = (plantable_area / subsite_area) * 100)

avg_table <- land %>%
  group_by(updtd_ln) %>%
  summarise(mean_pct = mean(plantable_pct, na.rm = TRUE),
            se_pct   = sd(plantable_pct, na.rm = TRUE) / sqrt(n()))

ggplot() +
  geom_jitter(data = land,
              aes(x = updtd_ln, y = plantable_pct),
              width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = avg_table,
             aes(x = updtd_ln, y = mean_pct, color = updtd_ln),
             size = 3) +
  geom_errorbar(data = avg_table,
                aes(x = updtd_ln, ymin = mean_pct - se_pct, ymax = mean_pct + se_pct, color = updtd_ln),
                width = 0.4, linewidth = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  labs(x = "Green Space Type", y = "% Plantable Area")


#Second question - Basal area
#Calculate basal area for each subsite, then run model

trees <- st_read("output/trees012825_DBH.shp") %>% 
  st_drop_geometry()

trees <- trees %>%
  mutate(basal_area = pi * (DBH_fnl/200)^2)

subsite_ba <- trees %>%
  group_by(subsite, updtd_ln, Plot) %>%
  summarise(basal_area = sum(basal_area, na.rm = TRUE), .groups = "drop")

subsite_ba <- subsite_ba %>%
  left_join(land %>% select(subsite, subsite_area, plantable_area), by = "subsite") %>%
  mutate(basal_area_plant_ha = basal_area / (plantable_area/10000)) %>%
  mutate(basal_area_ha = basal_area / (subsite_area/10000))

avg_ba <- subsite_ba %>%
  group_by(updtd_ln) %>%
  summarise(mean_ba = mean(basal_area_ha, na.rm = TRUE),
            se_ba   = sd(basal_area_ha, na.rm = TRUE) / sqrt(n()),
            mean_plant_ba = mean(basal_area_plant_ha, na.rm = TRUE),
            se_plant_ba   = sd(basal_area_plant_ha, na.rm = TRUE) / sqrt(n()))

#BASAL AREA PER SUBSITE AREA
ggplot() +
  geom_jitter(data = subsite_ba,
              aes(x = updtd_ln, y = basal_area_ha),
              width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = avg_ba,
             aes(x = updtd_ln, y = mean_ba, color = updtd_ln),
             size = 3) +
  geom_errorbar(data = avg_ba,
                aes(x = updtd_ln, ymin = mean_ba - se_ba, ymax = mean_ba + se_ba, color = updtd_ln),
                width = 0.4, linewidth = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  labs(x = "Green Space Type", y = "Basal Area (m²/ha)")


#BASAL AREA PER PLANTABLE AREA
ggplot() +
  geom_jitter(data = subsite_ba,
              aes(x = updtd_ln, y = basal_area_plant_ha),
              width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = avg_ba,
             aes(x = updtd_ln, y = mean_plant_ba, color = updtd_ln),
             size = 3) +
  geom_errorbar(data = avg_ba,
                aes(x = updtd_ln, ymin = mean_plant_ba - se_plant_ba, ymax = mean_plant_ba + se_plant_ba, color = updtd_ln),
                width = 0.4, linewidth = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  labs(x = "Green Space Type", y = "Basal Area (m²/ha)")

#Median DBH

# Subsite-level median DBH
subsite_dbh <- trees %>%
  group_by(subsite, updtd_ln, Plot) %>%
  summarise(median_dbh = median(DBH_fnl, na.rm = TRUE), .groups = "drop")

avg_dbh <- subsite_dbh %>%
  group_by(updtd_ln) %>%
  summarise(mean_dbh = mean(median_dbh, na.rm = TRUE),
            se_dbh   = sd(median_dbh, na.rm = TRUE) / sqrt(n()))

ggplot() +
  geom_jitter(data = subsite_dbh,
              aes(x = updtd_ln, y = median_dbh),
              width = 0.1, alpha = 0.3, color = "gray40", size = 1.5) +
  geom_point(data = avg_dbh,
             aes(x = updtd_ln, y = mean_dbh, color = updtd_ln),
             size = 3) +
  geom_errorbar(data = avg_dbh,
                aes(x = updtd_ln, ymin = mean_dbh - se_dbh, ymax = mean_dbh + se_dbh, color = updtd_ln),
                width = 0.4, linewidth = 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  ) +
  labs(x = "Green Space Type", y = "Median DBH (cm)")

#Appendix table for basal area and tree abundance per species

species_table <- trees %>%
  select(-species) %>% 
  rename(species = gns_spc) %>% 
  group_by(species) %>%
  summarise(
    stem_abundance = n(),
    basal_area = sum(basal_area, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(       
    percent_total_stems = round(stem_abundance / sum(stem_abundance) * 100, 1),
    percent_total_basal_area = round(basal_area / sum(basal_area) * 100, 1)
  ) %>%
  arrange(desc(stem_abundance))

species_table

write.csv(species_table, "output/SI_A2_species_table.csv", row.names = FALSE)
