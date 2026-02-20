
# Editing Names -----------------------------------------------------------

treedata <- read.csv("input/Inventoried_Trees2023_Raw.csv", header = TRUE)

unique_names <- unique(newtreedata$Essence_latin_w_cultivar)

print(unique_names)

#So there are still names in the Essence_latin_w_cultivar column that contain the words HEDGE", "EDGE", "Hedge", "START", "Start". I want to remove these words from the names so that I can see how many species I have. 

library(stringr)

# Define the words to remove
words_to_remove <- c("HEDGE", "EDGE", "Hedge", "START", "Start", "EMD")

# Create a regular expression pattern to match any of the specified words
pattern <- paste0("\\b", paste(words_to_remove, collapse = "|"), "\\b")

# Remove the specified words from the Essence_latin_w_cultivar column
newtreedata$Essence_latin_w_cultivar <- str_replace_all(newtreedata$Essence_latin_w_cultivar, pattern, "")

#Some names still have a random "H" at the end, so I remove these

newtreedata$Essence_latin_w_cultivar <- str_replace(newtreedata$Essence_latin_w_cultivar, "(_*H)?$", "")

newtreedata$Essence_latin_w_cultivar <- str_replace(newtreedata$Essence_latin_w_cultivar, "([H_]+)$", "")

unique_names1 <- unique(newtreedata$Essence_latin_w_cultivar)

print(unique_names1)

