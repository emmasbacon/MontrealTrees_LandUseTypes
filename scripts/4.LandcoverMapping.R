#Landuse May 21 2024 

source("R/download_shp.R")
library(sf)
library(dplyr)
library(tidyr)
library(mapview)
library(readr)


# Download land cover -----------------------------------------------------

# download the shapefiles
shps <- download_shp('https://observatoire.cmm.qc.ca/documents/geomatique/UtilisationDuSol/2022_Shapefiles/66-US-2022.zip',
                     'input/landuse/')

# get all zip files within the folder
zips <- list.files(path = "input/landuse/", pattern = "*.zip", full.names = TRUE)
# unzip all the files
unzip <- lapply(zips, FUN = function(x){unzip(x, exdir = 'input/landuse/')})
# get names of all .shp files
shps_list <- list.files(path = 'input/landuse/', pattern = "*.shp", full.names = TRUE)
# read list of all shapefiles
shps <- lapply(shps_list, read_sf)
# merge all shapefiles
merge <-  do.call(rbind, shps) %>%
  select(c("UTIL_SOL", "geometry"))


# Prep sampling plots -----------------------------------------------------
trees <- read.csv("input/Urbantrees_April24.csv") %>%
  drop_na(c("Latitude", "Longitude")) %>%
  st_as_sf(coords = c("Latitude", "Longitude"), crs = 4326) %>%
  # transform to land cover projection
  st_transform(crs = st_crs(merge))

plots <- read.csv("input/Pollen_traps.csv", sep = ";") %>%
  drop_na(c("latitude", "longitude")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(merge)) %>%
  # 200 m buffers
  st_buffer(200) %>%
  # area of the plot
  mutate(plot_area = st_area(geometry))

# Extract land cover ------------------------------------------------------

trees_lc <- st_intersection(trees, merge) %>%
  mutate(land_cover = case_when(UTIL_SOL == '100' | UTIL_SOL == '101' | UTIL_SOL == "102" | UTIL_SOL == "112" | UTIL_SOL == "103" | UTIL_SOL == "113" | UTIL_SOL == "104"| UTIL_SOL == "114" ~ 'résidentielle' ,
                                UTIL_SOL == '200' ~ "commerciale",
                                UTIL_SOL == '300' ~ "bureau",
                                UTIL_SOL == '400' ~ "industrie",
                                UTIL_SOL == '500'| UTIL_SOL == '510' | UTIL_SOL == '520' ~ "institutionnelle",
                                UTIL_SOL == '600' ~ "parc",
                                UTIL_SOL == '700' | UTIL_SOL == '710' | UTIL_SOL == '720' | UTIL_SOL == '725' | UTIL_SOL == '750' | UTIL_SOL == '760' ~ "utilité publique",
                                UTIL_SOL == '800' ~ "agricole",
                                UTIL_SOL == '900' ~ "terrain vacant",
                                UTIL_SOL == '1000' ~ "hydrographie",
                                UTIL_SOL == '1100' ~ "golf",
                                .default = 'unknown'))

plots_lc <- st_intersection(plots, merge) %>%
  mutate(land_cover = case_when(UTIL_SOL == '100' | UTIL_SOL == '101' | UTIL_SOL == "102" | UTIL_SOL == "112" | UTIL_SOL == "103" | UTIL_SOL == "113" | UTIL_SOL == "104"| UTIL_SOL == "114" ~ 'résidentielle' ,
                                UTIL_SOL == '200' ~ "commerciale",
                                UTIL_SOL == '300' ~ "bureau",
                                UTIL_SOL == '400' ~ "industrie",
                                UTIL_SOL == '500'| UTIL_SOL == '510' | UTIL_SOL == '520' ~ "institutionnelle",
                                UTIL_SOL == '600' ~ "parc",
                                UTIL_SOL == '700' | UTIL_SOL == '710' | UTIL_SOL == '720' | UTIL_SOL == '725' | UTIL_SOL == '750' | UTIL_SOL == '760' ~ "utilité publique",
                                UTIL_SOL == '800' ~ "agricole",
                                UTIL_SOL == '900' ~ "terrain vacant",
                                UTIL_SOL == '1000' ~ "hydrographie",
                                UTIL_SOL == '1100' ~ "golf",
                                .default = 'unknown')) %>%
  group_by(Trap, land_cover) %>%
  summarize(plot_area = first(plot_area),
            geometry = st_union(geometry),
            lc_area = st_area(geometry),
            lc_per = round((lc_area/plot_area)*100, 3))

#Aside, need to delete all Reynoutria japonica individuals since they are not actually trees

trees_lc <- subset(trees_lc, Essence_latin_w_cultivar != "Reynoutria japonica")

#Delete existing columns: Category, Essence_fr, Essence_en

trees_lc <- trees_lc %>% select(-Essence_fr, -Essence_en, -Category)

#Replace spaces in Comment column with NAs

trees_lc$Comment[trees_lc$Comment == ""] <- NA

#Redo OBJECTID

trees_lc <- mutate(trees_lc, NewOBJECTID = rownames(trees_lc))


# Save --------------------------------------------------------------------

write_sf(trees_lc, "output/trees_landcover.shp")
write_sf(plots_lc, "output/plots_landcover.shp")

write_csv(trees_lc, 'output/cleantreedata_may23.csv')

mapview(trees_lc)
