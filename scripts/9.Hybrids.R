#Determining which hybrids are distinct enough

library("dplyr")
library("sf")
library("stringr")
library("tidyr")

trees <- st_read("output/trees012125.shp")

unique_hybrid_essn_names <- trees %>%
  filter(hybrid == "yes") %>%       
  group_by(Essn___) %>%            
  summarise(n_individuals = n()) %>%
  arrange(Essn___)  

# View the results
unique_hybrid_essn_names

#Tilia x mongolica into Tilia mongolica (mistake in naming, it is not a hybrid)

trees <- trees %>%
  mutate(across(where(is.character), ~ str_replace_all(., regex("Tilia x Mongolica|Tilia x mongolia|Tilia x mongolica|Tilia mongolica 'Harvest Gold'", ignore_case = TRUE), "Tilia mongolica")))

trees <- trees %>%
  mutate(hybrid = ifelse(Essn___ == "Tilia mongolica", "no", hybrid))

#Change hybrids and add them into the gns_spc based on them being unique enough to warrant being their own species

#Lots of Ulmus hybrids and cultivars. I searched up each hybrid and determined whether I could group them together based on them being essentially the same species. 

ulmus_trees <- trees %>%
  filter(str_detect(Essn___, "Ulmus")) %>%  
  distinct(Essn___) %>%                    
  pull(Essn___)

ulmus_trees

#27 ulmus species or hybrids or cultivars
#Categories:
#Ulmus x Morton
#Ulmus x hollandica: add in Ulmus x 'Patriots'
#Ulmus x New Horizon
#Ulmus x 'Frontier' and Ulmus x Frontier" 
#Ulmus glabra
#Ulmus americana
#Ulmus wilsoniana
#Ulmus pumila: Ulmus x 'Homestead', Ulmus x 'Sapporo Autumn Gold' (is just a cultivar)
#Ulmus rubra
#Ulmus davidiana: Ulmus x "Cathedral'

trees <- trees %>%
  mutate(Essn___ = case_when(
    Essn___ == "Ulmus x 'Patriots'" ~ "Ulmus x hollandica",
    Essn___ == "Ulmus x 'Frontier'" ~ "Ulmus x Frontier",
    Essn___ == "Ulmus x 'Homestead'" ~ "Ulmus pumila",
    Essn___ == "Ulmus x 'Sapporo Autumn Gold'" ~ "Ulmus pumila",
    Essn___ == "Ulmus x 'Cathedral'" ~ "Ulmus davidiana",
    TRUE ~ Essn___  # Keep other species unchanged
  ))



#Do the same with Malus 

malus_trees <- trees %>%
  filter(str_detect(Essn___, "Malus")) %>%  
  distinct(Essn___) %>%                    
  pull(Essn___)

malus_trees

#Considering these as the Malus species categories

#Malus spp: Malus domestica, Malus dolgo, Malus x 'Dolgo'
#Malus sylvestris (crabapple): Malus angustifolia (most likely a mistake since this tree isn't found this north), Malus ioensis (same mistake)
#Malus hupehensis (crabapple)
#Malus baccata
#Malus x floribunda: Malus floribunda, change hybrid to yes - is its own distinct hybrid so I am not lumping it with all other hybrids
#Malus hybrids: Malus x 'Makamik', Malus x 'Prairiefire', Malus makamik, Malus x 'Golden Raindrops', Malus x 'Royalty', Malus x 'Indian Magic', Malus x 'Red Splendor', Malus x Centurion®, Malus x 'Radiant', 
#Malus hybrids cont: Malus x Harvest Gold'¿ === ALSO APART OF MALUS SPP. 
#Malus prunifolia

trees <- trees %>%
  mutate(Essn___ = case_when(
    Essn___ %in% c("Malus domestica", "Malus dolgo", "Malus x 'Dolgo'") ~ "Malus spp.",
    Essn___ %in% c("Malus angustifolia", "Malus ioensis") ~ "Malus sylvestris",
    Essn___ == "Malus floribunda" ~ "Malus x floribunda",
    Essn___ %in% c("Malus x 'Makamik'", "Malus x 'Prairifire'", "Malus makamik", "Malus x 'Golden Raindrops'", "Malus x 'Royalty'", "Malus x 'Indian Magic'", "Malus x 'Red Splendor'", "Malus x Centurion®", "Malus x 'Radiant'", "Malus x 'Harvest Gold'¿") ~ "Malus hybrids",
    TRUE ~ Essn___  # Keep other species unchanged
  ))

trees <- trees %>%
  mutate(hybrid = ifelse(Essn___ == "Malus x floribunda", "yes", hybrid))

#Check gns_spc column

malus_tree <- trees %>%
  filter(str_detect(gns_spc, "Malus")) %>%  
  distinct(gns_spc) %>%                    
  pull(gns_spc)

malus_tree

#Need to change to mirror Essn___ changes

trees <- trees %>%
  mutate(gns_spc = case_when(
    gns_spc %in% c("Malus domestica", "Malus dolgo") ~ "Malus spp.",
    gns_spc %in% c("Malus angustifolia", "Malus ioensis") ~ "Malus sylvestris",
    gns_spc == "Malus floribunda" ~ "Malus x floribunda",
    gns_spc %in% c("Malus makamik", "Malus hybrids") ~ "Malus spp.",
    TRUE ~ gns_spc  # Keep other species unchanged
  ))

#All other hybrids are okay to be moved to the gns_spc column as species. 
#Add Essn___ name to gns_spc without cultivar

trees <- trees %>%
  mutate(gns_spc = case_when(
    hybrid == "yes" & is.na(gns_spc) ~ str_remove(Essn___, "\\s*'[^']+'"),  # Remove cultivar in single quotes
    TRUE ~ gns_spc  # Keep other values unchanged
  ))

#Now, the only NAs are the trees that were only identified at the genus level. We need to determine a way to extrapolate the trees to a species. 
#Method: assign genus to the most abundant species in that genus. 

#First how many trees are identified only to the genus level: 

num_missing_gns_spc <- trees %>%
  filter(!is.na(genus) & is.na(gns_spc)) %>%
  nrow()

num_missing_gns_spc #759

#Table with number of trees in each genus (that are not identified to the spc level), how many total are in each of those genus, and what the most common species is


get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

genus_table <- trees %>%
  filter(!is.na(genus)) %>%
  group_by(genus) %>%
  summarize(
    Num_Rows_NA_gns_spc = sum(is.na(gns_spc)),  # Count rows with NA in gns_spc
    Total_Rows = n(),  # Total rows for each genus
    Most_Common_gns_spc = get_mode(gns_spc)  # Most common gns_spc
  ) %>%
  filter(Num_Rows_NA_gns_spc > 0)

#Replace NA in gns_spc for each of those genus with the most common species. 

most_common_gns_spc <- with(genus_table, 
                            setNames(Most_Common_gns_spc, genus))

trees <- trees %>%
  mutate(
    gns_spc = case_when(
      is.na(gns_spc) ~ most_common_gns_spc[genus],  # Replace NA with the most common species for that genus
      TRUE ~ gns_spc  # Keep the existing gns_spc if it's not NA
    )
  )

#Fix Malus, Crateagus, and Photinia

#Most common value for Malus is NA

malus_spc <- trees %>%
  filter(stringr::str_detect(gns_spc, regex("Malus", ignore_case = TRUE))) %>%
  group_by(gns_spc) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count))

#Most common is Malus spp. (106), followed by Malus hupehensis (101). I will stick with the rule and change them to Malus spp. 
#There is only 1 tree that is genus Photinia and the gns_spc is NA. Change gns_spc to Photinia. 

trees <- trees %>%
  mutate(
    gns_spc = case_when(
      genus == "Malus" & is.na(gns_spc) ~ "Malus spp.",  # For Malus, replace NA with "Malus spp."
      genus == "Photinia" & is.na(gns_spc) ~ "Photinia",  # For Photinia, replace NA with "Photinia"
      TRUE ~ gns_spc  # Keep other rows unchanged
    )
  )


#For crataegus, I want to distribute the NAs evenly between the species. There are 13 NAs and 9 species with 1-3 individuals each

#Most common value for Crataegus is NA, but the rest are very evenely spread (1-3 per species). However, it is notoriously hard to 
#ID Crateagus, and most have few individuals, so I will make them all Crateagus spp

trees <- trees %>%
  mutate(
    gns_spc = case_when(
      genus == "Crataegus" ~ "Crataegus spp.", 
      TRUE ~ gns_spc  
    )
  )

write_sf(trees, 'output/trees012825.shp')
