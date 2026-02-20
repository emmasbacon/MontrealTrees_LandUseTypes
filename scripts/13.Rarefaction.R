#Rarefaction

library("sf")
library("dplyr")
library("tidyr")
library("iNEXT")
library("tibble")

trees <- st_read("output/trees012825.shp")

## import packages
library(iNEXT)
library(ggplot2)


# GST Level - No Subsites -------------------------------------------------

names <- levels(as.factor(trees$updtd_l))

treelist <- trees %>% 
  st_drop_geometry() %>% 
  count(updtd_l, gns_spc) %>% 
  pivot_wider(id_cols = gns_spc, names_from = updtd_l, values_from = n, 
              values_fill = 0) %>% 
  drop_na(gns_spc) %>% 
  column_to_rownames(., var = 'gns_spc')

#Rarify using iNEXT package - coverage-based

#sample coverage (SC) is in the $DataInfo frame that is output)

Out <- iNEXT(treelist, datatype = "abundance", q = 0)

gginextplot <- ggiNEXT(Out, type = 1)

gginextplot

gginextplot + coord_cartesian(xlim = c(0,2000)) #to Zoom in 

ggnextsc <- ggiNEXT(Out, type = 2)

ggnextsc

ggnextcb <- ggiNEXT(Out, type = 3)

ggnextcb

#Compute diversity estimates of order q = 0, 1, 2 
#for abundance data with a sample coverage of 75%
#if instead you put NULL for the level, 
#this function will compute the diversity estimates for the 
#minimum sample size/coverage among all sites)

est <- estimateD(treelist, datatype = 'abundance', base = "coverage", level = NULL, conf=0.95)

est


# GST_Overview_CoverageBased ----------------------------------------------

library(iNEXT)
library(tidyverse)

# Prepare data (your existing code)
treelist <- trees %>% 
  st_drop_geometry() %>% 
  count(updtd_l, gns_spc) %>% 
  pivot_wider(id_cols = gns_spc, names_from = updtd_l, values_from = n, 
              values_fill = 0) %>% 
  drop_na(gns_spc) %>% 
  column_to_rownames(., var = 'gns_spc')

# Run iNEXT for q=0 (species richness), can add q=1 if you want
Out <- iNEXT(treelist, datatype = "abundance", q = 0, se = TRUE, nboot = 200, knots = 500)

# Plot coverage-based rarefaction/extrapolation curves:
ggnextcb <- ggiNEXT(Out, type = 3) +
  coord_cartesian(xlim = c(0, 0.95)) + # zoom into 95% coverage max
  labs(
    title = "Coverage-based Rarefaction Curves",
    x = "Sample Coverage",
    y = "Species Richness"
  ) +
  theme_minimal()

print(ggnextcb)

# Optional: compute diversity estimates at fixed coverage levels (e.g., 0.95)
est <- estimateD(treelist, datatype = 'abundance', base = "coverage", level = 0.95, conf = 0.95)
print(est)

# GST_SUBSITES_ABUNDANCE --------------------------------------------------

library("mobr")
library("purrr")

trees <- st_read("output/trees012825.shp") %>% 
  st_drop_geometry()

#Separate into subsites (i.e. 01A_Commercial, 17C_Parc)

trees$subsite <- paste(trees$Plot, trees$updtd_l, sep = "_")

species_matrix <- trees %>%
  group_by(subsite, gns_spc) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = gns_spc, values_from = count, values_fill = 0)

species_matrix <- species_matrix %>%
  column_to_rownames("subsite") 

subsite_info <- trees %>%
  distinct(subsite, updtd_l)

# Add green space type to species matrix
species_df <- species_matrix %>%
  rownames_to_column("subsite") %>%
  left_join(subsite_info, by = "subsite")

#Split by subsite

subsite_list <- species_df %>%
  group_by(subsite) %>%
  group_split() %>%
  set_names(map_chr(., ~ unique(.x$subsite)))

#remove subsite and GST columns, keep only species counts
subsite_list <- map(subsite_list, ~ .x %>%
                      select(-subsite, -updtd_l) %>%
                      select(where(is.numeric)) %>%
                      as.data.frame())


# Remove species columns with zero counts in each subsite (some may be empty)
subsite_list <- map(subsite_list, function(df) {
  df[, colSums(df) > 0, drop = FALSE]
})

#Run iNEXT for species richness, Hill effective number of species, and Hill Dominance
#nboot = 200 common default for resampling, but redo on lab computer with 500

inext_abund <- map(subsite_list, ~ {
  abund_vector <- as.numeric(.x[1, ])
  names(abund_vector) <- colnames(.x)
  iNEXT(abund_vector, datatype = "abundance", q = c(0, 1, 2), nboot = 200)
})

summary_subsites <- map2_dfr(inext_abund, names(inext_abund), function(res, subsite_name) {
  # Check if AsyEst exists and has expected rows
  asy <- res$AsyEst
  
  if (is.null(asy) || nrow(asy) != 3) {
    warning(paste("Skipping", subsite_name, "due to unexpected AsyEst structure."))
    return(NULL)
  }
  
  # Add q labels
  div_labels <- c("Species richness", "Shannon diversity", "Simpson diversity")
  
  # Bind labels only if they match the number of rows
  asy <- asy %>%
    mutate(
      Diversity = div_labels[seq_len(nrow(.))], 
      subsite = subsite_name,
      metric = case_when(
        Diversity == "Species richness" ~ "Species Richness",
        Diversity == "Shannon diversity" ~ "Hill q=1",
        Diversity == "Simpson diversity" ~ "Hill q=2",
        TRUE ~ NA_character_
      )
    ) %>%
    select(subsite, metric, mean = Estimator, se = Est_s.e.)
  
  return(asy)
})

# Add GST info back to summary_subsites for plotting and grouping
summary_subsites <- summary_subsites %>%
  left_join(subsite_info, by = "subsite") %>%
  rename(GreenSpace = updtd_l)

#Hurlbert PIE

pie_table <- map2_dfr(subsite_list, names(subsite_list), function(mat, subsite_name) {
  pies <- apply(mat, 1, function(x) calc_PIE(x))
  tibble(subsite = subsite_name,
         PIE = pies)
})

pie_table <- pie_table %>%
  left_join(subsite_info, by = "subsite") %>%
  rename(GreenSpace = updtd_l)

pie_subsites <- pie_table %>%
  mutate(metric = "PIE", value = PIE) %>%
  select(subsite, GreenSpace, metric, value)

summary_long <- summary_subsites %>%
  mutate(value = mean) %>%
  select(subsite, GreenSpace, metric, value)

#Combine all

combined_subsites <- bind_rows(summary_long, pie_subsites)
#combined_summary <- bind_rows(meanse_summary, pie_summary)

saveRDS(combined_subsites, 'output/rare_combined_subsites.rds')


# GST_SUBSITES_COVERAGE ---------------------------------------------------

inext_coverage <- map(subsite_list, ~ {
  abund_vector <- as.numeric(.x[1, ])
  names(abund_vector) <- colnames(.x)
  iNEXT(abund_vector,
        q = c(0, 1, 2),
        datatype = "abundance",
        se = TRUE,
        nboot = 200,
        conf = 0.95)
})

#Extract diversity at 95% coverage 

# Apply estimateD for coverage-based estimates
sumcov_subsites <- map2_dfr(subsite_list, names(subsite_list), function(x, subsite_name) {
  abund_vector <- as.numeric(x[1, ])
  names(abund_vector) <- colnames(x)
  
  estimateD(abund_vector,
            datatype = "abundance",
            base = "coverage",
            level = 0.95,
            conf = 0.95) %>%
    mutate(
      se = (qD.UCL - qD.LCL) / (2 * 1.96),
      metric = case_when(
        Order.q == 0 ~ "Species Richness",
        Order.q == 1 ~ "Hill q=1",
        Order.q == 2 ~ "Hill q=2"
      ),
      subsite = subsite_name
    ) %>%
    select(subsite, metric, value = qD, se)
})

#PIE coverage based 

#Calculate sample size for 0.95 coverage
sample_sizes <- map(subsite_list, function(x) {
  abund_vector <- as.numeric(x[1, ])
  names(abund_vector) <- colnames(x)
  
  estimateD(abund_vector,
            datatype = "abundance",
            base = "coverage",
            level = 0.95,
            conf = 0.95) %>%
    pull(m)  # m is the sample size corresponding to 0.95 coverage
})

#Rarefy each community to that sample size, then calculate PIE
pie_cov <- map2_dfr(subsite_list, names(subsite_list), function(x, subsite_name) {
  abund_vector <- as.numeric(x[1, ])
  names(abund_vector) <- colnames(x)
  
  # Use rarefaction sample size (first value from sample_sizes)
  sample_size <- round(sample_sizes[[subsite_name]][1])
  
  # Rarefy the abundance vector
  rar_counts <- rrarefy(matrix(abund_vector, nrow = 1), sample = sample_size)
  
  # Calculate PIE
  pie_val <- calc_PIE(rar_counts[1, ])
  
  tibble(
    subsite = subsite_name,
    metric = "PIE",
    value = pie_val
  )
})

#Combine with other metrics
pie_cov <- pie_cov %>%
  left_join(subsite_info, by = "subsite") %>%
  rename(GreenSpace = updtd_l)

sumcov_subsites <- sumcov_subsites %>%
  left_join(subsite_info, by = "subsite") %>%
  rename(GreenSpace = updtd_l) %>%
  select(subsite, GreenSpace, metric, value)

combcov_subsites <- bind_rows(sumcov_subsites, pie_cov)

saveRDS(combcov_subsites, 'output/rare_coverage_subsites.rds')


# Differences between Abundance vs. Coverage vs. Observed  ----------------

diversity_subsites <- readRDS("output/diversitymetrics_table_rare.rds")

div_long <- diversity_subsites %>%
  select(subsite, GreenSpace, richness, Rare_rich, Cov_rich) %>%
  pivot_longer(cols = c(richness, Rare_rich, Cov_rich),
               names_to = "method", values_to = "richness") %>%
  mutate(method = recode(method,
                         richness = "observed",
                         Rare_rich = "abundance",
                         Cov_rich = "coverage"))

aov_rich <- aov(richness ~ method + Error(subsite/method), data = div_long)
summary(aov_rich)


resids <- residuals(aov_rich[["subsite:method"]])
hist(resids, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(resids)
qqline(resids, col = "red")
shapiro.test(resids)

#Not normal, so Friedman
friedman.test(richness ~ method | subsite, data = div_long)

#Pairwise wilcox test to see which methods differ
pairwise.wilcox.test(div_long$richness, div_long$method, 
                     paired = TRUE, p.adjust.method = "BH")





