#Census Map

library("dplyr")
library("sf")
library("tidyr")

#Get plots 

plots <- read.csv("input/Pollen_traps.csv", sep = ";") %>%
  drop_na(c("latitude", "longitude")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = 3347) %>%
  # 200 m buffers
  st_buffer(200) %>%
  # area of the plot
  mutate(plot_area = st_area(geometry))

#Functions

download_csv <- function(url){
  temp <- tempfile()
  download.file(url, temp, mode = "wb")
  master <- as.character(unzip(temp, list = TRUE)$Name)
  df <- read.csv(unz(temp, master[1]))
  return(df)
}


download_shp <- function(url, dest){
  temp <- tempfile()
  download.file(url, dest, mode = "wb")
  shp <- st_read(file.path("/vsizip", dest))
  return(shp)
}

#Download DA data

da_raw <-  download_shp("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip", "input/da_raw.zip")

#Download Census data 2021

census_raw <- download_csv("https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/download-telecharger/comp/GetFile.cfm?Lang=E&FILETYPE=CSV&GEONO=006_Quebec")

da_raw_area <- da_raw %>% 
  mutate(area_total = st_area(geometry))


study_da <- st_transform(plots, st_crs(da_raw)) %>%
  st_intersection(da_raw_area) %>%
  mutate(DAUID = as.integer(DAUID),
         areaint = st_area(geometry)/1000000) # area of the DA that intersects w the buffer (sq km)

census_da_f <- census_raw %>%
  select(c("ALT_GEO_CODE","CHARACTERISTIC_ID","C1_COUNT_TOTAL")) %>%
  rename(DAUID = "ALT_GEO_CODE",
         sofac = "CHARACTERISTIC_ID",
         sonum = "C1_COUNT_TOTAL") %>%
  right_join(study_da, by = "DAUID") %>%
  filter(sofac %in% c(1, 6:7, 115, 111, 1998, 2008, 1683, 1684, 1697, 1527, 1534, 41, 42, 43, 1402, 1403))

# we are interested in education, language, income, and ethnicity

# 1   Population 2021
# 6   Pop density per sq km
# 7   Land area sq km

# Income statistics in 2020 for the population aged 15 years and over in private households - 100% data (10)
# 115   Median after-tax income in 2020 among recipients ($)

# Knowledge of official languages for the total population excluding institutional residents - 100% data (36)
# 384   English only
# 385   French only
# 386   English and French
# 387   Neither English nor French

# 1527    Total - Immigrant status and period of immigration for the population in private households - 25% sample data (79)
# 1528      Non-immigrants (80)
# 1529      Immigrants (81)

# 1544    Total - Place of birth for the immigrant population in private households - 25% sample data (85)
# 1545      Americas
# 1557      Europe
# 1574      Africa
# 1585      Asia
# 1603      Oceania and other places of birth (93)

# 2014    Total - Highest certificate, diploma or degree for the population aged 25 to 64 years in private households - 25% sample data (165)
# 2015      No certificate, diploma or degree
# 2016      High (secondary) school diploma or equivalency certificate (167)
# 2017      Postsecondary certificate, diploma or degree


census_da_w <- census_da_f %>% pivot_wider(names_from = sofac, values_from = sonum)

census_da_r <- census_da_w %>%
  rename(totpop = "1") %>%
  rename(popdens = "6") %>%
  rename(area = "7") %>%
  rename(medinc = "115") %>%
  rename(tot_inc = "111") %>%
  rename(tot_edu = "1998") %>%
  rename(tot_bac = "2008") %>%
  rename(tot_min = "1683") %>%
  rename(pop_min = "1684") %>%
  rename(non_min = "1697") %>%
  rename(tot_imm = "1527") %>%
  rename(imm_rec = "1534") %>%
  rename(tot_dwe = "41") %>%
  rename(sin_dwe = "42") %>%
  rename(sem_dwe = "43") %>%
  rename(tot_ind = "1402") %>%
  rename(ide_ind = "1403")


census_da_sf <- st_as_sf(census_da_r, sf_column_name = c("geometry"), crs = st_crs(study_da))

census_da_na <- census_da_sf %>%
  drop_na(Trap)  %>%
  filter(drop_units(area_total) > 0) %>%
  mutate(da = as.factor(DAUID)) %>%
  mutate(across(c(totpop:ide_ind), ~as.numeric(.))) %>%
  select(-c(DGUID, LANDAREA, PRUID, DAUID))

# population percentages
can_cen_pp <- census_da_na %>%
  mutate(per_bac = tot_bac/tot_edu,
         per_min = pop_min/tot_min,
         per_non_min = non_min/tot_min,
         per_imm = imm_rec/tot_imm,
         per_sin_dwe = sin_dwe/tot_dwe,
         per_sem_dwe = sem_dwe/tot_dwe,
         per_sin_sem = (sin_dwe + sem_dwe)/tot_dwe,
         per_ind = ide_ind/tot_ind)



# need to calculate the area of the DA that is within the neigbourhood (areaint)
study_cen_pop <- can_cen_pp %>%
  # to calculate the approximate population within the neighbourhood bounds (assuming equal density throughout the DA)
  # divide the intersected area/total area of DA and multiply the population by that
  # can then use this population as weight for weighted means
  mutate(popwithin = (as.numeric(areaint)/as.numeric(area_total))*as.numeric(totpop)) %>%
  select(c("Trap","da","geometry","totpop", "popwithin", "area", "area_total", "areaint", "popdens",
           "medinc", "per_bac", "per_min", "per_non_min", "per_imm", "per_sin_dwe", "per_sem_dwe", "per_sin_sem", "per_ind"))

# population weighted mean
study_cen <- study_cen_pop %>% 
  group_by(Trap) %>% 
  mutate(across(popdens:per_ind, ~ weighted.mean(as.numeric(.x), as.numeric(popwithin), na.rm = T))) %>% 
  summarize(DAcount = n(),
            totarea = drop_units(sum(areaint)),
            geometry = st_union(geometry),
            popwithin = sum(as.numeric(popwithin)),
            popdens = first(popdens),
            medinc = first(medinc),
            per_bac = first(per_bac),
            per_min = first(per_min),
            per_non_min = first(per_non_min), 
            per_imm = first(per_imm),
            per_sin_dwe = first(per_sin_dwe),
            per_sem_dwe = first(per_sem_dwe),
            per_sin_sem = first(per_sin_sem),
            per_ind = first(per_ind)
            ) %>%
  distinct(Trap, .keep_all = TRUE) %>% 
  left_join(.,st_drop_geometry(plots),by = "Trap") %>%
  select(-c(Adress, Full_coord, plot_area))
  


write_sf(study_cen, 'output/census.gpkg')

#Histogram to look at distribution of variables

ggplot(study_cen, aes(x = per_bac)) + geom_histogram(bins = 10)
  