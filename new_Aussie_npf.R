####################
### Script to incorporate Helen's new Australian non-plant-feeding list
### Jan 16, 2020;  Script by Rachael Blake
####################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) ; library(here) ; library(taxize) ; library(rgbif)

# source the custom functions 
source("./custom_taxonomy_funcs.R")

# set working directory
setwd("/nfs/insectinvasions-data")

##########
# read in existing Australia non-plant-feeding file and *clean*
aus_npf_file <- here("data/raw_data/raw_by_country/Australia_NON-NATIVE_PLANT_FEEDERS_ants.xlsx")

aus_npf <- separate_taxonomy(aus_npf_file) 

aus_npf_v <- aus_npf %>% 
             distinct(genus_species) %>% 
             unlist(., use.names = FALSE) 

# send to GBIF
aus_npf_l <- lapply(aus_npf_v, get_accepted_taxonomy) 

xtra_cols <- c("kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
               "note", "familykey", "genuskey", "scientificname", "canonicalname", "confidence")

# make dataframe of all results
suppressMessages(
aus_acc <- aus_npf_l %>% 
           purrr::reduce(full_join) %>% 
           mutate(genus_species = str_squish(genus_species)) %>% 
           select(-one_of(xtra_cols))
)

##########
# read in Helen's new Australian npf file
HN_npf_file <- read_excel(here("data/raw_data/allCOL_HN13Jan2020.xlsx"), trim_ws = TRUE, col_types = "text") 

new_npf <- HN_npf_file %>% 
           select(Species, Order, Family, Australia) %>% 
           filter(!is.na(Australia)) %>% 
           rename(genus_species = Species, ORDER = Order, FAMILY = Family) %>% 
           select(-Australia)

# now need to *clean*, but can't use separate_taxonomy()
new_npf_v <- new_npf %>% 
             mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
                    genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
                    genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
                    genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
                    genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
                    genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
                    genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\.", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
                    genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
                    genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
                    ) %>% 
             distinct(genus_species) %>% 
             unlist(., use.names = FALSE)

# send to GBIF
new_npf_l <- lapply(new_npf_v, get_accepted_taxonomy) 

# make dataframe of all results
suppressMessages(
new_aus_acc <- new_npf_l %>% 
               purrr::reduce(full_join) %>% 
               mutate(genus_species = str_squish(genus_species)) %>% 
               select(-one_of(xtra_cols))
)
  
##########
# Hrm, 64 new cases, not 13 like I expected from Rebecca's e-mail
new_npf_no_match <- anti_join(new_aus_acc, aus_acc, by = "genus_species")
nrow(new_npf_no_match)
View(new_npf_no_match)


# combine cases
npf_combo <- full_join(aus_npf, new_npf, by = "genus_species")



