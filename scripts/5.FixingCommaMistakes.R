source('scripts/0.packages.R')

#Open ArcGIS Data and Raw Data
Rawtreedata <- read.csv("input/Inventoried_Trees2023_Raw.csv", header = TRUE) %>%
  select(OBJECTID, DBH_final)

# replace virgules in raw dataset 
rawtreedata_dec <- Rawtreedata %>% 
  mutate(DBH_final = str_replace(DBH_final, ",", "."))

rawtree <- rawtreedata_dec %>% 
  mutate(DBH_final = as.numeric(DBH_final))

arcdata <- read.csv("input/TreeDataJun12FromArc.csv", header = TRUE) %>%
  mutate(DBH_fnl = as.numeric(DBH_fnl))

# separate this year and last year's data 
fielddata <- arcdata %>% 
  filter(is.na(OBJECTI))

lastyear <- arcdata %>% 
  filter(!is.na(OBJECTI))

# join data together
joined <- left_join(lastyear, rawtree, by = join_by("OBJECTI" == "OBJECTID"))

# if DBH_fnl = " " from arc replace it with DBH_final from raw data (some DBH_final and DBH_fnl are different because I changed them during the field)
joined$DBH_fnl <- ifelse(is.na(joined$DBH_fnl) | joined$DBH_fnl == " ", 
                         joined$DBH_final, 
                         joined$DBH_fnl)

# replace all commas with periods 
# replace all spaces w nothing
joined_dbh <- joined %>%
  mutate(DBH_fnl = str_replace(DBH_fnl, ",", "."),
         DBH_fnl = str_replace(DBH_fnl, " ", ""),
         DBH_fnl = as.numeric(DBH_fnl)) %>%
  mutate_if(is.character, na_if, " ")

# read in plot data 
plots <- read.csv("input/Pollen_traps.csv", sep = ";") %>%
  drop_na(c("latitude", "longitude")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(crs = 32188) %>%
  # 200 m buffers
  st_buffer(200) %>% 
  st_transform(4326)

field_blanks <- fielddata %>% 
  mutate_if(is.character, na_if, "") %>%
  st_as_sf(., coords = c("Long", "Lat"), crs = 4326) %>% 
  st_make_valid()

# intersect trees w plots 
field_plots <- st_intersection(field_blanks, plots) %>% 
  mutate(Plot = Trap)

# put everything back together 
joined_sel <- joined_dbh %>% 
  select(-DBH_final)

field_sel <- field_plots %>% 
  select(-c(Trap, Adress, District, Full_coord)) %>% 
  mutate(Long = sf::st_coordinates(.)[,1],
         Lat = sf::st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  select(c("Long", "Lat", "X", "Plot", "OBJECTI", "Essn___", "DBH_fnl", "DBH_UQA", "DBH_VMT", "H_S_L_W",
           "Hdg_NmS", "M_M__E_", "Comment", "Citizen", "rowid", "Hedg_YN", "genus", "species", "NOBJECT",
           "hybrid", "cultivr", "gns_spc", "UTIL_SO", "lnd_cvr"))

full <- rbind(joined_sel, field_sel) %>% 
  st_as_sf(., coords = c("Long", "Lat"), crs = 4326)


write_sf(full, 'output/treedataupdatedjun13.shp')
