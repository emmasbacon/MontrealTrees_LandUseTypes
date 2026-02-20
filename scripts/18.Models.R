#Models 

library(dplyr)
library(tidyr)
library(tibble)
library(sf)


combined_plotdat <- readRDS("output/combined_plotdat.rds")

combined_plotdat <- combined_plotdat %>%
  mutate(medinc_center = medinc - mean(medinc, na.rm = TRUE)) %>%
  mutate(residential_plant_prop_center = residential_plant_prop - mean(residential_plant_prop, na.rm = TRUE))

#without plot as a random effect because of how data is structured
model_resinc <- lm(species_richness ~ residential_area_prop * medinc, data = combined_plotdat)
summary(model_resinc)

model_resinccenter <- lm(species_richness ~ areapropcentred * medinccentred, data = combined_plotdat)
summary(model_resinccenter)

vif(model_resinc)

# Check VIF
vif(model_resinc, type = "predictor")

#Check res + inc
model_check <- lm(species_richness ~ residential_area_prop + medinc, data = combined_plotdat)
summary(model_check)

#Other models

model_treeresinccenter <- lm(tree_count ~ areapropcentred * medinccentred, data = combined_plotdat)
summary(model_treeresinccenter)

model_treemed <- lm(tree_count ~ medinc, data = combined_plotdat)
summary(model_treemed)

ggplot(combined_plotdat, aes(x = medinc, y = tree_count)) +
  geom_point(alpha = 0.8, size = 5, colour = "forestgreen") +  
  geom_smooth(method = "lm", color = "black", se = FALSE) +  
  labs(
    x = "Revenu moyen ($)",
    y = "Nombre d'arbres"
  ) +
  theme_minimal(base_size = 16)


# Metric ~ medinc ---------------------------------------------------------

model_res <- lm(species_richness ~ residential_area_prop, data = combined_plotdat)
summary(model_res) #Sig

model_incab <- lm(tree_count ~ medinc, data = combined_plotdat)
summary(model_incab) #Not sig, but almost 0.0613
  
model_inc <- lm(species_richness ~ medinc, data = combined_plotdat)
summary(model_inc) #Not sig

model_incENS <- lm(hill_q1 ~ medinc, data = combined_plotdat)
summary(model_incENS) #Not sig
  
model_incEVE <-lm(pie_eve ~ medinc, data = combined_plotdat)
summary(model_incEVE) #Not sig, but almost at 0.0513


# Metric ~ built area proportion ------------------------------------------



mod_built <- lm(species_richness ~ blt_prp, data = combined_plotdat)
summary(mod_built)

mod_bENS <- lm(hill_q1 ~ blt_prp, data = combined_plotdat)
summary(mod_bENS)

mod_bEVE <- lm(pie_eve ~ blt_prp, data = combined_plotdat)
summary(mod_bEVE)

mod_abbuilt <- lm(tree_count ~ blt_prp, data = combined_plotdat)
summary(mod_abbuilt)

model_private <- lm(species_richness ~ priv_treeprop, data = combined_plotdat)
summary(model_private)


#Plot

library(ggplot2)

#Mod_medinc_tree_richesse_together

long_medinc <- combined_plotdat %>%
  select(medinc, tree_count, richness, hill_q1, pie_eve) %>%
  pivot_longer(
    cols = c(tree_count, richness, hill_q1, pie_eve),
    names_to = "metric",
    values_to = "value"
  )

long_medinc$metric <- recode(long_medinc$metric,
                          tree_count = "Number of Trees",
                          richness = "Species Richness",
                          hill_q1 = "Effective Number of Species",
                          pie_eve = "Evenness (PIE)"
                          
)

#Set custom facet order
long_medinc$metric <- factor(long_medinc$metric,
                             levels = c(
                               "Number of Trees",
                               "Species Richness",
                               "Effective Number of Species",
                               "Evenness (PIE)"
                             )
)

ggplot(long_medinc, aes(x = medinc, y = value, color = metric)) +
  geom_point(alpha = 0.8, size = 7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  facet_wrap(~metric, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Number of Trees" = "steelblue3",
      "Species Richness" = "steelblue3",
      "Effective Number of Species" = "steelblue3",
      "Evenness (PIE)" = "steelblue3"
    )
  ) +
  labs(
    x = "Median Income ($)",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 20)

#Mod_res
ggplot(combined_plotdat, aes(x = residential_area_prop, y = species_richness)) +
  geom_point(alpha = 0.8, size = 5, colour = "forestgreen") +  
  geom_smooth(method = "lm", color = "black", se = FALSE) +  
  labs(
    x = "Proportion de terrain résidentiel",
    y = "Richesse"
  ) +
  theme_minimal(base_size = 16)

#Mod_resinc
ggplot(combined_plotdat, aes(x = residential_area_prop, y = species_richness, color = medinc)) +
  geom_point(alpha = 0.8, size = 5) +  # Make points larger and more visible
  geom_smooth(method = "lm", color = "black", se = TRUE) +  # Single trend line
  scale_color_viridis_c(option = "plasma", guide = guide_colorbar(title = "Median Income")) +  # More distinct colors
  labs(
    x = "Proportion de terrain résidentiel",
    y = "Richesse"
  ) +
  theme_minimal(base_size = 16)

long_residential <- combined_plotdat %>%
  select(residential_area_prop, medinc, tree_count, richness, hill_q1, pie_eve) %>%
  pivot_longer(
    cols = c(tree_count, richness, hill_q1, pie_eve),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(metric,
                    tree_count = "Number of Trees",
                    richness = "Species Richness",
                    hill_q1 = "Effective Number of Species",
                    pie_eve = "Evenness (PIE)"
    ),
    metric = factor(metric, levels = c(
      "Number of Trees",
      "Species Richness",
      "Effective Number of Species",
      "Evenness (PIE)"
    ))
  )

ggplot(long_residential, aes(x = residential_area_prop, y = value, color = medinc)) +
  geom_point(alpha = 0.8, size = 7) +
  geom_smooth(method = "lm", color = "black", se = TRUE) +
  scale_color_viridis_c(option = "plasma", guide = guide_colorbar(title = "Median Income")) +
  facet_wrap(~metric, scales = "free_y") +
  labs(
    x = "Proportion of residential land",
    y = NULL
  ) +
  theme_minimal(base_size = 20)

#Mod_built

long_built <- combined_plotdat %>%
  select(blt_prp, tree_count, richness, hill_q1, pie_eve) %>%
  pivot_longer(
    cols = c(tree_count, richness, hill_q1, pie_eve),
    names_to = "metric",
    values_to = "value"
  )

long_built$metric <- recode(long_built$metric,
                             tree_count = "Number of Trees",
                             richness = "Species Richness",
                             hill_q1 = "Effective Number of Species",
                             pie_eve = "Evenness (PIE)"
                             
)

long_built$metric <- factor(long_built$metric,
                             levels = c(
                               "Number of Trees",
                               "Species Richness",
                               "Effective Number of Species",
                               "Evenness (PIE)"
                             )
)

ggplot(long_built, aes(x = blt_prp, y = value, color = metric)) +
  geom_point(alpha = 0.8, size = 6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~metric, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Number of Trees" = "orangered1",
      "Species Richness" = "orangered1",
      "Effective Number of Species" = "orangered1",
      "Evenness (PIE)" = "orangered1"
    )
  ) +
  labs(
    x = "Proportion of built area",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 20)

#Together

long_dat <- combined_plotdat %>%
  select(blt_prp, tree_count, species_richness) %>%
  pivot_longer(
    cols = c(tree_count, species_richness),
    names_to = "metric",
    values_to = "value"
  )
long_dat$metric <- recode(long_dat$metric,
                          tree_count = "Nombre d'arbres",
                          species_richness = "Richesse"
)

ggplot(long_dat, aes(x = blt_prp, y = value, color = metric)) +
  geom_point(alpha = 0.8, size = 7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  facet_wrap(~metric, scales = "free_y") +
  scale_color_manual(
    values = c(
      "Nombre d'arbres" = "forestgreen",
      "Richesse" = "forestgreen"
    )
  ) +
  labs(
    x = "Proportion de la placette qui est une surface bâtie",
    y = NULL,
    color = NULL  # Removes legend title if desired
  ) +
  theme_minimal(base_size = 20)

#Mod_priv
ggplot(combined_plotdat, aes(x = priv_treeprop, y = species_richness)) +
  geom_point(alpha = 0.8, size = 5, colour = "#59a96a") +  
  geom_smooth(method = "lm", color = "black", se = FALSE) +  
  labs(
    x = "Proportion d'arbres trouvés sur des terrains privés",
    y = "Richesse"
  ) +
  theme_minimal(base_size = 16)

#Built area, prop public/private street/res
#1. tree abundance as a function of built land (tree counts ~ proportion public trees ~ blt_prp) 
#2. proportion of trees on public land (pubtree_prop) ~ of blt_prp, 
#3. proportion of trees on private land privtree_prop ~ blt_prp, 
#4. proportion of trees that are street trees as a function of proportion of built area, and 
#5. proportion of trees that are on private land as a function of proportion of built area

plot_data <- combined_plotdat %>%
  pivot_longer(cols = c(public_treeprop, priv_treeprop, streettree_prop, resitree_prop),
               names_to = "variable", values_to = "value")

ggplot(plot_data, aes(x = blt_prp, y = value)) +
  geom_point(aes(color = variable), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~variable, scales = "fixed") +
  labs(x = "Built Land Proportion", y = "Response Variable", title = "Tree Metrics vs Built Land Proportion") +
  theme_minimal()

#Just private_pub

priv_pub <-combined_plotdat %>%
  st_drop_geometry() %>%
  pivot_longer(cols = c(public_treeprop, priv_treeprop),
               names_to = "variable", values_to = "value")

ggplot(priv_pub, aes(x = blt_prp, y = value, color = variable)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, aes(color = variable)) +
  scale_color_manual(
    values = c(
      public_treeprop = "violetred4",
      priv_treeprop = "forestgreen"
    ),
    labels = c(
      public_treeprop = "Public Land",
      priv_treeprop = "Private Land"
    )
  ) +
  labs(
    x = "Proportion of built area within plot",
    y = "Proportion of total trees",
    color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

#Models

model_tree_count <- lm(tree_count ~ blt_prp, data = combined_plotdat)
summary(model_tree_count) #sig

model_public_treeprop <- lm(public_treeprop ~ blt_prp, data = combined_plotdat)
summary(model_public_treeprop) #sig

model_privtree_prop <- lm(priv_treeprop ~ blt_prp, data = combined_plotdat)
summary(model_privtree_prop) #sig

model_street_tree_prop <- lm(streettree_prop ~ blt_prp, data = combined_plotdat)
summary(model_street_tree_prop) #sig

model_resi_land_prop <- lm(resitree_prop ~ blt_prp, data = combined_plotdat)
summary(model_resi_land_prop) #sig


#CSEE Models

#Slide 21



#Slide 22

model_resinc <- lm(species_richness ~ residential_area_prop * medinc, data = combined_plotdat)
summary(model_resinc)


model_2 <- lm(tree_count ~ residential_area_prop * medinc, data = combined_plotdat)
summary(model_2)


model_3 <- lm(hill_q1 ~ residential_area_prop * medinc, data = combined_plotdat)
summary(model_3)


model_4 <- lm(pie_eve ~ residential_area_prop * medinc, data = combined_plotdat)
summary(model_4)



#Slide 23

mod_abbuilt <- lm(tree_count ~ blt_prp, data = combined_plotdat)
summary(mod_abbuilt)

mod_built <- lm(species_richness ~ blt_prp, data = combined_plotdat)
summary(mod_built)

mod_bENS <- lm(hill_q1 ~ blt_prp, data = combined_plotdat)
summary(mod_bENS)

mod_bEVE <- lm(pie_eve ~ blt_prp, data = combined_plotdat)
summary(mod_bEVE)


#Slide 24

model_public_treeprop <- lm(public_treeprop ~ blt_prp, data = combined_plotdat)
summary(model_public_treeprop) #sig

model_privtree_prop <- lm(priv_treeprop ~ blt_prp, data = combined_plotdat)
summary(model_privtree_prop) #sig

