#Ordination


# PCoA 1 ------------------------------------------------------------------

#PCoA (Principal Coordinates Analysis) ordination plot 
#based on tree species composition across different green space types

#TDLR: Green Space Type - Ordination

library(vegan)
library(sf)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(ape)
library(ggplot2)


####SEP 23

library(vegan)
library(ape)
library(ggplot2)
library(dplyr)
library(tibble)
library(emmeans)

# 1. Compute Jaccard distance (presence/absence)
dist_jaccard <- vegdist(species_matrix, method = "jaccard")

# 2. PCoA using ape::pcoa
pcoa_res <- pcoa(dist_jaccard, correction = "cailliez")  # handles negative eigenvalues

# Extract site coordinates for first two axes
pcoa_df <- as.data.frame(pcoa_res$vectors[, 1:2])
colnames(pcoa_df) <- c("PCoA1", "PCoA2")
pcoa_df$Green_Space <- metadata$Green_Space

# % variance explained by axes
pcoa_var <- pcoa_res$values$Relative_eig[1:2] * 100

# 3. Plot PCoA
ggplot(pcoa_df, aes(x = PCoA1, y = PCoA2, color = Green_Space)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "dashed") +
  labs(
    x = paste0("PCoA Axis 1 (", round(pcoa_var[1], 1), "%)"),
    y = paste0("PCoA Axis 2 (", round(pcoa_var[2], 1), "%)"),
    color = "Green Space Type"
  ) +
  theme_minimal()

# 4. Beta-dispersion
disp <- betadisper(dist_jaccard, metadata$Green_Space)

# Test for differences in dispersion
anova(disp)

# Boxplot of distances to group centroid
boxplot(disp, main = "Beta Dispersion (Jaccard)")

# 5. Pairwise Tukey HSD
tukey_res <- TukeyHSD(disp)

# Convert to tidy format for ggplot
tukey_df <- as.data.frame(tukey_res$group) %>%
  rownames_to_column(var = "comparison") %>%
  rename(diff = diff, lwr = lwr, upr = upr, p.adj = `p adj`)

# Plot Tukey HSD results
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



####

library(vegan)
data(dune)
data(dune.env)

scores(rda(dune))

plot(capscale(vegdist(species_matrix, "bray") ~ metadata$Green_Space))

plot(capscale(vegdist(dune, "bray") ~ dune.env$Use, comm=dune))


#p.adjust function with either Bonferroni (strictest) or Holm/Hochberg (moderate)

#Anderson, M.J., Ellingsen, K.E. & McArdle, B.H. (2006) Multivariate dispersion as a measure of beta diversity. Ecology Letters 9, 683–693.

##Jaccard
# 1. Compute Jaccard distance
dist_jaccard <- vegdist(species_matrix, method = "jaccard")  # presence/absence-based

# 2. Beta-dispersion (homogeneity of multivariate dispersions)
disp_jaccard <- betadisper(dist_jaccard, metadata$Green_Space)

# 3. Overall test for differences in dispersion
anova(disp_jaccard)

# 4. Boxplot of distances to group centroid
boxplot(disp_jaccard, main = "Beta Dispersion (Jaccard)")

# 5. Pairwise Tukey HSD test
tukey_jaccard <- TukeyHSD(disp_jaccard)
tukey_jaccard

#plot(TukeyHSD(betadisper(vegdist(dune), dune.env$Use)))

# 1. Run CAP (constrained PCoA)
cap_model <- capscale(vegdist(species_matrix, method = "jaccard") ~ Green_Space, data = metadata)

# 2. Extract site scores for the first two axes
cap_scores <- scores(cap_model, display = "sites")
cap_df <- as.data.frame(cap_scores[, 1:2])
cap_df$Green_Space <- metadata$Green_Space

# 3. Extract % variance explained by axes
cap_var <- summary(cap_model)$cont$importance[2, 1:2] * 100

# 4. Plot with ggplot2
ggplot(cap_df, aes(x = CAP1, y = CAP2, color = Green_Space)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "dashed") +
  labs(
    x = paste0("CAP Axis 1 (", round(cap_var[1], 1), "%)"),
    y = paste0("CAP Axis 2 (", round(cap_var[2], 1), "%)"),
    color = "Green Space Type"
  ) +
  theme_minimal()

#Betadisp

dist_mat <- vegdist(species_matrix, method = "bray")
disp <- betadisper(dist_mat, metadata$Green_Space)
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


####


trees <- st_read("output/trees012825_DBH.shp")
trees_nogeo <- st_drop_geometry(trees)

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
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "dashed") +
  labs(
    x = paste0("PCoA Axis 1 (", round(percent_var[1], 1), "%)"),
    y = paste0("PCoA Axis 2 (", round(percent_var[2], 1), "%)"),
    color = "Green Space Type"
  ) +
  theme_minimal()








#Species fit with cmdscale (for arrows)
cmd_res <- cmdscale(dist_matrix, k = 2, eig = TRUE)
#Fit species vectors with envfit
species_fit <- envfit(cmd_res, species_matrix, perm = 9999)

saveRDS(species_fit, 'output/species_fit.rds')

species_fit <- readRDS("output/species_fit.rds")

#Extract species scores
species_scores_all <- as.data.frame(scores(species_fit, display = "vectors"))
colnames(species_scores_all)[1:2] <- c("PC1", "PC2")
species_scores_all$species <- rownames(species_scores_all)
species_scores_all$r <- species_fit$vectors$r
species_scores_all$pval <- species_fit$vectors$pvals

# Scale arrows to match PCoA axes of the first plot
mult_all <- vegan:::ordiArrowMul(species_fit)
species_scores_all <- species_scores_all %>%
  mutate(PCoA1 = PC1 * mult_all,
         PCoA2 = PC2 * mult_all)

#Plot with the top three pval species

top3_species <- species_scores_all %>%
  filter(pval < 0.001) %>%
  arrange(desc(r)) %>%
  slice_head(n = 3)

ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2, color = Green_Space)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "dashed") +
  geom_segment(data = top3_species,
               aes(x = 0, y = 0, xend = PCoA1, yend = PCoA2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black") +
  geom_text(data = top3_species,
            aes(x = PCoA1, y = PCoA2, label = species),
            color = "black", vjust = -0.5, size = 3) +
  labs(
    x = paste0("PCoA Axis 1 (", round(percent_var[1], 1), "%)"),
    y = paste0("PCoA Axis 2 (", round(percent_var[2], 1), "%)"),
    color = "Green Space Type"
  ) +
  theme_minimal() +
  coord_equal()

#Plot with just top 10 pval species

top10_species <- species_scores_all %>%
  filter(pval < 0.001) %>%
  arrange(desc(r)) %>%
  slice_head(n = 10)

ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2, color = Green_Space)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "dashed") +
  geom_segment(data = top10_species,
               aes(x = 0, y = 0, xend = PCoA1, yend = PCoA2),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black") +
  geom_text(data = top10_species,
            aes(x = PCoA1, y = PCoA2, label = species),
            color = "black", vjust = -0.5, size = 3) +
  labs(
    x = paste0("PCoA Axis 1 (", round(percent_var[1], 1), "%)"),
    y = paste0("PCoA Axis 2 (", round(percent_var[2], 1), "%)"),
    color = "Green Space Type"
  ) +
  theme_minimal() +
  coord_equal()



#Plot with just arrows (but all significant)
sig_species <- species_scores_all %>%
  filter(pval < 0.001)

ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2, color = Green_Space)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "dashed") +
  geom_segment(data = sig_species,
               aes(x = 0, y = 0, xend = PCoA1, yend = PCoA2, size = r),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black") +
  geom_text(data = sig_species,
            aes(x = PCoA1, y = PCoA2, label = species),
            color = "black", vjust = -0.5, size = 3) +
  scale_size_continuous(range = c(0.5, 2)) +
  labs(
    x = paste0("PCoA Axis 1 (", round(percent_var[1], 1), "%)"),
    y = paste0("PCoA Axis 2 (", round(percent_var[2], 1), "%)"),
    color = "Green Space Type",
    size = "Correlation r"
  ) +
  theme_minimal() +
  coord_equal()


#Plot with species only arrows, all species
all_species <- species_scores_all

ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2, color = Green_Space)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(aes(group = Green_Space), level = 0.9, linetype = "dashed") +
  geom_segment(data = all_species,
               aes(x = 0, y = 0, xend = PCoA1, yend = PCoA2, size = r),
               arrow = arrow(length = unit(0.3, "cm")),
               color = "black") +
  geom_text(data = all_species,
            aes(x = PCoA1, y = PCoA2, label = species),
            color = "black", vjust = -0.5, size = 3) +
  scale_size_continuous(range = c(0.5, 2)) +
  labs(
    x = paste0("PCoA Axis 1 (", round(percent_var[1], 1), "%)"),
    y = paste0("PCoA Axis 2 (", round(percent_var[2], 1), "%)"),
    color = "Green Space Type",
    size = "Correlation r"
  ) +
  theme_minimal() +
  coord_equal()


# Ordination Model --------------------------------------------------------

#NEED HELP HELP HELP

#Test for within-group variation differs between green space types 
#if group dispersions are unequal, can affect the validity of PERMANOVA
betadisp <- betadisper(dist_matrix, metadata$Green_Space)
anova(betadisp)
#F value = 10.34 and p-value ≈ 6.2e-08 
#indicate significant differences in dispersion among green spaces
#because this was sig, gllvm is better

#PERMANOVA adonis2

adonis2(
  dist_matrix ~ Green_Space,
  data = metadata,
  strata = metadata$Plot, 
  permutations = 999
)

#green space explains 21.3% of the variation in species composition.
#F = 5.13 and p-value = 0.001 

#BUT, it seems like gllvm might be better for species and random effects? 
#Let's try it

library(gllvm)
# Species counts matrix
species_data <- as.matrix(species_matrix)
all(rownames(species_data) == metadata$subsite_id)

#How to choose if poisson vs negative binomial
row_means <- rowMeans(species_data)
row_vars <- apply(species_data, 1, var)

plot(row_means, row_vars)
abline(0, 1, col = "red", lty = 2)

#If most points are above the red line, you have overdispersion → use Negative Binomial
#If they’re close to the line, Poisson might be fine.
#Use NB

mod_nb <- gllvm(
  y = species_data,
  family = "negative.binomial",
  Xformula = ~ Green_Space,
  num.lv = 5
)

summary(mod_nb)

