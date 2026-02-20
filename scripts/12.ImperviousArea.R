# Adding in canopy 

source("R/download_shp.R")
library(sf)
library(dplyr)
library(tidyr)
library(mapview)
library(readr)
library(terra)

# download the shapefiles
shps <- download_shp('https://observatoire.cmm.qc.ca/documents/carte/canope/2023/CF_SHP/660_CouvertForestier_2023_SHP.zip', 'input/forestcov/')

# get all zip files within the folder
zips <- list.files(path = "input/forestcov/", pattern = "*.zip", full.names = TRUE)
# unzip all the files
unzip <- lapply(zips, FUN = function(x){unzip(x, exdir = 'input/forestcov/')})
# get names of all .shp files
shps_list <- list.files(path = 'input/forestcov/', pattern = "*.shp", full.names = TRUE)
# read list of all shapefiles
shps <- lapply(shps_list, read_sf)
# merge all shapefiles
merge <-  do.call(rbind, shps) %>%
  select(c("sup_ha", "geometry"))


# Prep sampling plots -----------------------------------------------------

st_crs(merge) <- 4326

plots <- read.csv("input/Pollen_traps.csv", sep = ";") %>%
  drop_na(c("latitude", "longitude")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(merge)) %>%
  # 200 m buffers
  st_buffer(200) %>%
  # area of the plot
  mutate(plot_area = st_area(geometry))

