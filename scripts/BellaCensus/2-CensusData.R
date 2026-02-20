
# Packages ----------------------------------------------------------------
source('scripts/BellaCensus/0-packages.R')


# Functions ---------------------------------------------------------------

download_shp <- function(url, dest){
  
  temp <- tempfile()
  
  download.file(url, dest, mode = "wb")
  
  shp <- st_read(file.path("/vsizip", dest))
  
  return(shp)
  
}

download_csv <- function(url, dest){
  
  temp <- tempfile()
  
  download.file(url, dest, mode = "wb")
  
  master <- as.character(unzip(dest, list = TRUE)$Name)
  
  df <- read.csv(unz(dest, master[2]))
  
  return(df)
}

# Download Data -----------------------------------------------------------
options(timeout=120)

da <- download_shp('https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip', 
                   'input/census_da.zip')

# info page on CIMD data is here: https://www150.statcan.gc.ca/n1/pub/45-20-0001/452000012023002-eng.htm
cimd_raw <- download_csv('https://www150.statcan.gc.ca/n1/pub/45-20-0001/2023001/csv/qc_scores_quintiles_csv-eng.zip',
                         'input/census_cimd.zip')

# Intersect ---------------------------------------------------------------
# select DAs found in the QC CIMD dataset
cimd_raw$PRCDDA <- as.character(cimd_raw$PRCDDA)

cimd_da <- left_join(cimd_raw, da, by = c("PRCDDA" = "DAUID"))


# Save Datasets -----------------------------------------------------------
saveRDS(cimd_da, 'output/CIMD_DA.rds')

