#Data Cleaning Tree Inventory 

treedata <- read.csv("input/Inventoried_Trees2023_Raw.csv", header = TRUE)

summary(treedata)


# Cleaning ----------------------------------------------------------------


#Delete DBH_MS_calcul column

treedata$DBH_MS_calcul <- NULL

#Remove the transect rows, these are not trees, just markers used in GIS. 

treedata1 <- subset(treedata, !grepl("(\\b|_)((TRANSECT|Transect|T2|T1|T3|T4)(_\\w*)?)\\b", Essence_latin_w_cultivar))

#DBH_final and DBH_UQAM2022 are character not numeric, so change those to numerical

treedata1$DBH_final <- as.numeric(as.character(treedata1$DBH_final))

treedata1$DBH_UQAM2022 <- as.numeric(as.character(treedata1$DBH_UQAM2022))

#

summary(treedata1)

# Hedges - Turning hedges into individual rows ----------------------------

library(dplyr)

#First, there are hedges that have both START and END coordinates, but represent the same individuals. I will want to use these eventually to plot on GIS, but right now, let's delete the END entries so we do not have duplicates. 
#Some hedges have just a START or no START/END designation. Those are assumed to be solo entries/not duplicated. 

treedata2 <- subset(treedata1, !grepl("END", Essence_latin_w_cultivar))

#Separate hedges into individual rows using the # in the Hedge_NumberStems column

hedgedata <- treedata2 %>%
  filter(!is.na(Hedge_NumberStems) & is.numeric(Hedge_NumberStems)) %>% #The filter function is used to exclude rows where Hedge_NumberStems is NA or not numeric.
  group_by(rowid = row_number()) %>% #group the filtered data by creating a new column called "rowid" that contains the row number within each group
  slice(rep(row_number(), each = Hedge_NumberStems[1])) %>% 
  filter(!Plot %in% c("19B", "20C", "23A")) #replicates each row based on the value in the first row of the "Hedge_NumberStems" column

#Number of individual hedges

length(unique(na.omit(hedgedata$rowid)))

#now I have my treedata set and the hedgedata

newtreedata <- bind_rows(treedata2, hedgedata)

#I would like to keep track of which individuals are a part of a hedge, so I want to add a column that is HEDGE_YN

newtreedata <- mutate(newtreedata, Hedge_YN = ifelse(!is.na(Hedge_NumberStems), "Y", "N"))

#I want to order my data by Plot because it looks better

newtreedata$Plot <- factor(newtreedata$Plot, levels = c("01A", "02A", "03A", "04A", "05B", "06C", "07A", "08A", "09B", "10A", "11B", "12B", "12C", "13A", "14A", "15A", "16A", "17C", "18B", "19B", "20C", "21B", "22A", "23A", "24B"))

# Arrange the rows by the Plot column
newtreedata <- arrange(newtreedata, Plot)

saveRDS(newtreedata, 'output/newtreedata.rds')

