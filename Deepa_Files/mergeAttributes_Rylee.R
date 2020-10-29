library(stringdist) # install.packages("stringdist")
library(tidyverse)
## Establishment Data ##

# setwd("C:/Users/dpureswa/Desktop/R files/data/SESYNC")
#setwd("../../Data")

Establishments <- read.csv("./Deepa_Files/EstablishedInsects2020Oct16.csv")# ("EstablishedInsects2020Oct16.csv")
str(Establishments)
Origins <- read.csv("nfs_data/data/clean_data/attribute_table.csv")# ("attribute_table.csv")
str(Origins)

## Detect occurrences of genus_species in Establishments that do not occur in Origins, and suggest possible matches (likely typos) ##
# matchNotFound <- !(Establishments$genus_species %in% Origins$genus_species)
matchNotFound <- !(Establishments$user_supplied_name %in% Origins$user_supplied_name)
# missingTaxa <- data.frame(genus_species = Establishments[which(matchNotFound),]$genus_species)
missingTaxa <- data.frame(genus_species = Establishments[which(matchNotFound),]$user_supplied_name)
missingTaxa$possibleMatch <- Origins[amatch(missingTaxa$genus_species, Origins$user_supplied_name, method="osa", maxDist=4),]$genus_species
nApproxMatches = sum(!is.na(missingTaxa$possibleMatch), na.rm = TRUE)

## Merging establishment file with Origin information ##
## Add diagnostic columns ##
# Estab_with_Origins <- merge(Establishments, Origins, by="genus_species", all=T)
Estab_with_Origins <- full_join(Establishments, Origins, by = "user_supplied_name")
Estab_with_Origins$genus_species_mismatch <- Estab_with_Origins$user_supplied_name %in% missingTaxa$genus_species
# Estab_with_Origins <- merge(Estab_with_Origins, missingTaxa, by="genus_species", all=T)
Estab_with_Origins <- full_join(Estab_with_Origins, missingTaxa, by = c("user_supplied_name" = "genus_species"))

## Set "unknown" and "cryptogenic" origins to NA ##
Estab_with_Origins[which(tolower(Estab_with_Origins$origin) == "unknown"),]$origin = NA
Estab_with_Origins[which(tolower(Estab_with_Origins$origin) == "cryptogenic"),]$origin = NA
Estab_with_Origins$origin <- factor(Estab_with_Origins$origin)
levels(Estab_with_Origins$origin)

## Detect missing binary origins for entries that have text origins specified and which matched a taxon in the attributes table ##
Estab_with_Origins$missingBinaryOrigins <- is.na(Estab_with_Origins$origin_Nearctic)
Estab_with_Origins$uncodedOrigin <- !is.na(Estab_with_Origins$origin) & Estab_with_Origins$missingBinaryOrigins
nUncodedOrigins <-  sum(Estab_with_Origins$uncodedOrigin)
nUncodedOriginsWithMatch <-  sum(!Estab_with_Origins$genus_species_mismatch & Estab_with_Origins$uncodedOrigin, na.rm = TRUE)

## Summary of problems detected ##
cat(nrow(missingTaxa),
    " rows of establishment data have genus_species columns that do not match anything in the attributes table.\n",
    nApproxMatches,
    " cases seem to be caused by typos in the genus_species column.\n\n",
    nUncodedOrigins,
    " merged rows have uncoded binary origins (NAs) despite having text origins specified.\n",
    "Of these, ",
    nUncodedOriginsWithMatch,
    " merged correctly with the attributes table; NAs are present in the attributes table for these rows.\n",
    "The remaining ",
    (nUncodedOrigins - nUncodedOriginsWithMatch),
    " rows did not merge correctly; this may be due to genus_species typos or missing taxa in the attributes table.",
    sep=""
    )

## Save merged result ##
write.csv(Estab_with_Origins, file="C:/Users/dpureswa/Desktop/R files/data/SESYNC/Estab_with_Origins.csv")



###For Curculionidae
## subsetting

Estab_Curculionidae <- subset (Estab_with_Origins, family.x =="Curculionidae")
write.csv(Estab_Curculionidae, file="C:/Users/dpureswa/Desktop/R files/data/SESYNC/Estab_Curculionidae.csv")

## Interception for Curculionidae data ##

Interception_counts <- read.csv("Species_CountryInterceptionTable_Feb04.csv")
Intercep_Curculionidae <- subset (Interception_counts, family =="Curculionidae")
write.csv(Intercep_Curculionidae, file="C:/Users/dpureswa/Desktop/R files/data/SESYNC/Intercep_Curculionidae.csv")

