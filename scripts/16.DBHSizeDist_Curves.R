#DBH Size Distribution Curves

library(ggplot2)
library(patchwork)
library(dplyr)
library(sf)

trees <- st_read("output/trees012825_dbh.shp")

gs_colors <- c(
  "Commercial"           = "#F8766D",
  "Vacant Lot"        = "#B79F00",
  "Park"                 = "#00BA38",
  "Institutional"  = "#00BFC4",
  "Public Right-Of-Way"          = "#619CFF",
  "Residential"           = "#F564E3"
)


# Kernel density plot of DBH distribution by Green Space Type
# Panel A: Scaled kernel density (density × N), Smoothed count of trees
pA <- ggplot(data = trees,
             mapping = aes(x = DBH_fnl, group = updtd_ln, color = updtd_ln)) +
  geom_density(aes(y = after_stat(count)), size = 0.75, alpha = 0.2) +
  scale_color_manual(values = gs_colors) +
  theme_classic() +
  xlim(c(0, 175)) +
  labs(
    x = "DBH (cm)",
    y = "Density",
    color = "Green Space Type"
  ) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "white"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )


pA

#Same panel, DBH cutoff at 100 to better see patterns

ppA <- ggplot(data = trees,
             mapping = aes(x = DBH_fnl, group = updtd_ln, color = updtd_ln)) +
  geom_density(aes(y = after_stat(count)), size = 0.75, alpha = 0.2) +
  scale_color_manual(values = gs_colors) +
  theme_classic() +
  xlim(c(0, 75)) +
  labs(
    x = "DBH (cm)",
    y = "Density",
    color = "Green Space Type"
  ) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "white"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )


ppA

# WITHOUT HEDGES ----------------------------------------------------------


trees_nohedge <- trees %>%
  filter(Hedg_YN != "Y")

# Panel A: Scaled kernel density (density × N)
pA_hedge <- ggplot(data = trees_nohedge,
             mapping = aes(x = DBH_fnl, group = updtd_ln, color = updtd_ln)) +
  geom_density(aes(y = after_stat(count)), size = 0.75, alpha = 0.2) +
  scale_color_manual(values = gs_colors) +
  theme_classic() +
  xlim(c(0, 175)) +
  labs(
    x = "DBH (cm)",
    y = "Scaled Density (No. of Trees)",
    color = "Green Space Type"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

#same to 100 DBH

ppA_hedge <- ggplot(data = trees_nohedge,
                   mapping = aes(x = DBH_fnl, group = updtd_ln, color = updtd_ln)) +
  geom_density(aes(y = after_stat(count)), size = 0.75, alpha = 0.2) +
  scale_color_manual(values = gs_colors) +
  theme_classic() +
  xlim(c(0, 75)) +
  labs(
    x = "DBH (cm)",
    y = "Scaled Density (No. of Trees)",
    color = "Green Space Type"
  ) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

ppA_hedge


# WITHOUT RESIDENTIAL  ----------------------------------------------------

trees_nores <- trees %>%
  filter(updtd_ln != "Residential")

pC <- ggplot(data = trees_nores,
              mapping = aes(x = DBH_fnl, group = updtd_ln, color = updtd_ln)) +
  geom_density(aes(y = after_stat(count)), size = 0.75, alpha = 0.2) +
  scale_color_manual(values = gs_colors) +
  theme_classic() +
  xlim(c(0, 175)) +
  labs(
    x = "DBH (cm)",
    y = "Density",
    color = "Green Space Type"
  ) +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "white"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )
pC

#Same but DBH to 100

ppC <- ggplot(data = trees_nores,
             mapping = aes(x = DBH_fnl, group = updtd_ln, color = updtd_ln)) +
  geom_density(aes(y = after_stat(count)), size = 0.75, alpha = 0.2) +
  scale_color_manual(values = gs_colors) +
  theme_classic() +
  xlim(c(0, 75)) +
  labs(
    x = "DBH (cm)",
    y = "Density",
    color = "Green Space Type"
  ) +
  theme(
    legend.position = "none",  
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

ppC 

#Together with scaled density
pA / pC

#GRID ALL TREES
pA_grid <- ggplot(data = trees,
             mapping = aes(x = DBH_fnl, group = updtd_ln, color = updtd_ln)) +
  geom_density(aes(y = after_stat(count)), size = 0.75, alpha = 0.2) +
  scale_color_manual(values = gs_colors) +
  scale_x_continuous(breaks = seq(0, 175, by = 10)) +  # vertical grid lines every 10 cm
  labs(
    x = "DBH (cm)",
    y = "Density",
    color = "Green Space Type"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "white"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted")
  )


pA_grid

#GRID NO HEDGES 
pA_hedge_grid <- ggplot(data = trees_nohedge,
             mapping = aes(x = DBH_fnl, group = updtd_ln, color = updtd_ln)) +
  geom_density(aes(y = after_stat(count)), size = 0.75, alpha = 0.2) +
  scale_color_manual(values = gs_colors) +
  scale_x_continuous(breaks = seq(0, 175, by = 10)) +  
  labs(
    x = "DBH (cm)",
    y = "Density",
    color = "Green Space Type"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "white"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_line(color = "grey90", linetype = "dotted")
  )

pA_hedge_grid

#####FINAL FIGURE OPTIONS

#1 Overlayed hedges and no hedges on same panel

p_combined <- ggplot() +
  geom_density(
    data = trees,
    aes(
      x = DBH_fnl,
      y = after_stat(count),
      group = updtd_ln,
      color = updtd_ln,
      linetype = "All trees"
    ),
    linewidth = 0.75,
    key_glyph = "path"
  ) +
  geom_density(
    data = trees_nohedge,
    aes(
      x = DBH_fnl,
      y = after_stat(count),
      group = updtd_ln,
      color = updtd_ln,
      linetype = "Hedges removed"
    ),
    linewidth = 0.75,
    key_glyph = "path"
  ) +
  scale_color_manual(values = gs_colors) +
  scale_linetype_manual(values = c(
    "All trees" = "solid",
    "Hedges removed" = "44"  
  )) +
  scale_x_continuous(breaks = seq(0, 75, by = 10), limits = c(0, 75)) +
  labs(
    x = "DBH (cm)",
    y = "Density (scaled to number of trees)",
    color = "Land Use Type",
    linetype = "Data"
  ) +
  guides(
    color = guide_legend(order = 1),
    linetype = guide_legend(
      order = 2,
      override.aes = list(color = "black", size = 1.2),
      keywidth = 3  
    )
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "white"),
    axis.title = element_text(face = "bold", size = 18),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18)
  )

p_combined

#OPTION 2. Two panels

library(patchwork)

pA <- ggplot(data = trees,
             aes(x = DBH_fnl, y = after_stat(count),
                 group = updtd_ln, color = updtd_ln)) +
  geom_density(linewidth = 0.75, key_glyph = "path") +
  scale_color_manual(values = gs_colors) +
  scale_x_continuous(breaks = seq(0, 75, by = 10), limits = c(0, 75)) +
  labs(x = "DBH (cm)", y = "Density (scaled to number of trees)", color = "Green Space Type") +
  theme_classic() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.background = element_rect(color = "black", fill = "white"),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  )

pB <- ggplot(data = trees_nohedge,
             aes(x = DBH_fnl, y = after_stat(count),
                 group = updtd_ln, color = updtd_ln)) +
  geom_density(linewidth = 0.75, key_glyph = "path") +
  scale_color_manual(values = gs_colors) +
  scale_x_continuous(breaks = seq(0, 75, by = 10), limits = c(0, 75)) +
  labs(x = "DBH (cm)", y = "Density (scaled to number of trees)") +
  theme_classic() +
  theme(
    legend.position = "none",  
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(size = 10)
  )

p_combined_AB <- pA / pB + plot_annotation(tag_levels = "A")
p_combined_AB







    


