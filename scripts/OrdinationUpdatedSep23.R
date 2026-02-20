#NEW ORDINATION FILE

library(vegan)
library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ape)
library(ggplot2)
library(emmeans)

trees <- st_read("output/trees012825_DBH.shp")
trees_nogeo <- st_drop_geometry(trees)

gs_colors <- c(
  "Commercial"           = "#F8766D",
  "Vacant Lot"           = "#B79F00",
  "Park"                 = "#00BA38",
  "Institutional"        = "#00BFC4",
  "Public Right-Of-Way"  = "#619CFF",
  "Residential"          = "#F564E3"
)

species_matrix <- trees_nogeo %>%
  mutate(subsite_id = paste(Plot, updtd_ln, sep = "_")) %>%
  group_by(subsite_id, gns_spc) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = gns_spc, values_from = count, values_fill = 0) %>%
  column_to_rownames("subsite_id")

metadata <- data.frame(subsite_id = rownames(species_matrix), stringsAsFactors = FALSE) %>%
  mutate(
    Plot = str_extract(subsite_id, "^[^_]+"),
    Green_Space = str_extract(subsite_id, "(?<=_).+")
  ) %>%
  mutate(
    Plot = factor(Plot),
    Green_Space = factor(Green_Space)
  )

#Bray-Curtis dissimilarity between species across subsites
dist_matrix <- vegdist(species_matrix, method = "bray")

#PCoA with ape (variance explained + site coordinates)
pcoa_results <- pcoa(dist_matrix)
pcoa_coords <- as.data.frame(pcoa_results$vectors[, 1:2])
colnames(pcoa_coords) <- c("PCoA1", "PCoA2")
pcoa_coords <- pcoa_coords %>%
  rownames_to_column("subsite_id") %>%
  left_join(metadata, by = "subsite_id")

# Percent variance explained by each axis
percent_var <- 100 * (pcoa_results$values$Relative_eig)
percent_var[1:2] 
#Axis 1: 18.83%, Axis 2: 10.18%
cumulative_var <- cumsum(percent_var)
cumulative_var[1:2]  # cumulative percent explained by first 2 axes, so in total
#Axes 1 and 2 explain 29%

#Plot only points
ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2, color = Green_Space)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "solid") +
  scale_color_manual(values = gs_colors) +
  labs(
    x = paste0("Axis 1 (", round(percent_var[1], 1), "%)"),
    y = paste0("Axis 2 (", round(percent_var[2], 1), "%)"),
    color = "Land Use Type"
  ) +
  theme_minimal()






#Betadisper Bray-Curtis

disp <- betadisper(dist_matrix, metadata$Green_Space)
disp
boxplot(disp) 
anova(disp)

tukey_res <- TukeyHSD(disp)
tukey_res

tukey_df <- as.data.frame(tukey_res$group) %>%
  rownames_to_column(var = "comparison") %>%
  rename(diff = diff, lwr = lwr, upr = upr, p.adj = `p adj`)

ggplot(tukey_df, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    x = "Pairwise Green Space Comparison",
    y = "Difference in Beta Dispersion",
    title = "Tukey HSD on Beta Dispersion"
  ) +
  theme_minimal()


#Ensure rownames match between ordination and species data
pcoa_coords_envfit <- as.data.frame(pcoa_results$vectors[, 1:2])
colnames(pcoa_coords_envfit) <- c("PCoA1", "PCoA2")
rownames(pcoa_coords_envfit) <- rownames(species_matrix)
stopifnot(all(rownames(pcoa_coords_envfit) == rownames(species_matrix)))

#Fit species vectors
species_fit <- envfit(pcoa_coords_envfit, species_matrix, permutations = 9999)

#Extract scores, r², and p-values
species_scores <- as.data.frame(scores(species_fit, display = "vectors"))
species_scores$r2 <- species_fit$vectors$r
species_scores$pval <- species_fit$vectors$pvals
species_scores$species <- rownames(species_scores)

#Keep significant species
species_sig <- species_scores %>%
  filter(pval < 0.001) %>%
  mutate(
    dominant_axis = ifelse(abs(PCoA1) > abs(PCoA2), "Axis1", "Axis2")
  )

#Add total individuals per species
species_abundance <- trees_nogeo %>%
  group_by(gns_spc) %>%
  summarise(individuals = n(), .groups = "drop") %>%
  rename(species = gns_spc)

species_sig <- species_sig %>%
  left_join(species_abundance, by = "species")

# stimate which green space type each species correlates with
axis_means <- pcoa_coords %>%
  group_by(Green_Space) %>%
  summarise(mean_PC1 = mean(PCoA1), mean_PC2 = mean(PCoA2))

species_sig <- species_sig %>%
  rowwise() %>%
  mutate(
    correlated_GST = {
      if (abs(PCoA1) > abs(PCoA2)) {
        axis_means$Green_Space[which.max(sign(PCoA1) * axis_means$mean_PC1)]
      } else {
        axis_means$Green_Space[which.max(sign(PCoA2) * axis_means$mean_PC2)]
      }
    }
  ) %>%
  ungroup()

#Clean table
species_results_table <- species_sig %>%
  select(species, r2, pval, dominant_axis, correlated_GST, individuals) %>%
  arrange(pval)

# View results
species_results_table

species_results_table <- species_results_table %>%
  mutate(pval = formatC(pval, format = "f", digits = 3))

write.csv(species_results_table, "output/species_results_table.csv", row.names = FALSE)


#REPEAT FOR JACCARD
#Jaccard dissimilarity between species across subsites
dist_jac <- vegdist(species_matrix, method = "jaccard")

#PCoA with ape (variance explained + site coordinates)
pcoa_resultsj <- pcoa(dist_jac)
pcoa_coordsj <- as.data.frame(pcoa_resultsj$vectors[, 1:2])
colnames(pcoa_coordsj) <- c("PCoA1", "PCoA2")
pcoa_coordsj <- pcoa_coordsj %>%
  rownames_to_column("subsite_id") %>%
  left_join(metadata, by = "subsite_id")

# Percent variance explained by each axis
percent_varj <- 100 * (pcoa_resultsj$values$Relative_eig)
percent_varj[1:2] 
#Axis 1: 12.59%, Axis 2: 7.69%
cumulative_varj <- cumsum(percent_varj)
cumulative_varj[1:2]  # cumulative percent explained by first 2 axes, so in total
#Axes 1 and 2 explain 20.28%

#Plot only points
ggplot(pcoa_coordsj, aes(x = PCoA1, y = PCoA2, color = Green_Space)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "solid") +
  scale_color_manual(values = gs_colors) +
  labs(
    x = paste0("Axis 1 (", round(percent_varj[1], 1), "%)"),
    y = paste0("Axis 2 (", round(percent_varj[2], 1), "%)"),
    color = "Land Use Type"
  ) +
  theme_minimal()

#Betadisper

dispj <- betadisper(dist_jac, metadata$Green_Space)
dispj
boxplot(dispj) 
anova(dispj)

tukey_resj <- TukeyHSD(dispj)
tukey_resj

tukey_dfj <- as.data.frame(tukey_resj$group) %>%
  rownames_to_column(var = "comparison") %>%
  rename(diff = diff, lwr = lwr, upr = upr, p.adj = `p adj`)

ggplot(tukey_dfj, aes(x = comparison, y = diff)) +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    x = "Pairwise Green Space Comparison",
    y = "Difference in Beta Dispersion",
    title = "Tukey HSD on Beta Dispersion"
  ) +
  theme_minimal()


