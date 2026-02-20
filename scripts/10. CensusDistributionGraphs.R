#Making graphs to show the distribution of census variables accross plots

library("dplyr")
library("gridExtra")
library("ggplot2")
library("forcats")

census <- st_read("output/census.gpkg")

# Plot for medinc
ggplot(census, aes(x = fct_reorder(Trap, medinc), y = medinc)) +
  geom_point(size = 5, colour = "forestgreen") +
  labs(x = "Trap", y = "Median Income") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for per_bac
ggplot(census, aes(x = fct_reorder(Trap, per_bac), y = per_bac)) +
  geom_point(size = 5, colour = "forestgreen") +
  labs(x = "Trap", y = "Percentage with Bachelor's Degree") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for per_min
ggplot(census, aes(x = fct_reorder(Trap, per_min), y = per_min)) +
  geom_point(size = 5, colour = "forestgreen") +
  labs(x = "Trap", y = "Percentage of Minority Population") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for per_imm
ggplot(census, aes(x = fct_reorder(Trap, per_imm), y = per_imm)) +
  geom_point(size = 5, colour = "forestgreen") +
  labs(x = "Trap", y = "Percentage of Immigrants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for per_sin_dwe
ggplot(census, aes(x = fct_reorder(Trap, per_sin_dwe), y = per_sin_dwe)) +
  geom_point(size = 5, colour = "forestgreen") +
  labs(x = "Trap", y = "Percentage of Single Dwellings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot for per_ind
ggplot(census, aes(x = fct_reorder(Trap, per_ind), y = per_ind)) +
  geom_point(size = 5, colour = "forestgreen") +
  labs(x = "Trap", y = "Percentage of Indigenous Peoples") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
