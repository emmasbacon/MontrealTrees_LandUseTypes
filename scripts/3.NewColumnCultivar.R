
# 3. NewColumnCultivar ----------------------------------------------------

treesclean <- read.csv("input/TreeDataOpenRefineJan25.csv", header = TRUE)

#Now that we have the cleaner data with no typos from OpenRefine, I need to create a column called NewOBJECTID with a new order of ids for each individual in the dataset. 
#ObjectID column shows old object id that corresponds with the raw data. 

treesclean <- mutate(treesclean, NewOBJECTID = rownames(treesclean))

#I want to keep the Essence_latin_w_cultivar category, but I also want to separate the species into genus, species and cultivar/hybrid. So I have to duplicate the column

treesclean$Essence_latin <- treesclean$Essence_latin_w_cultivar

# sorting species name into genus, species, and cultivar columns

#Yes/No hybrid column

treesclean <- treesclean %>% 
  mutate(hybrid = case_when(str_detect(Essence_latin, " x ") == T  ~ 'yes',
                            str_detect(Essence_latin, " x ") == F ~ 'no'),
         Essence_latin = str_replace(Essence_latin, " x ", " "),
         cultivar = case_when(str_detect(Essence_latin, "'") == T ~ sub("[^\']+\'([^\']+).*", "\\1", treesclean$Essence_latin)))
#Remove "x" from hybrids in Essence_latin

#Separate genus and species
treesclean <- treesclean %>% separate(Essence_latin, c("genus","species"), sep = " ")

saveRDS(treesclean, 'output/urbantreesclean_April12.rds')

