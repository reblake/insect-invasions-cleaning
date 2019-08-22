####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean occurrence table          #####
##### created by Rachael Blake    11/19/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) ; library(countrycode)

# source the custom functions 
#source("./custom_taxonomy_funcs.R")

# set working directory
setwd("/nfs/insectinvasions-data")

# # checks to see if clean flat files exist, otherwise creates them from multi-worksheet files
# if(!file.exists("./data/raw_data/seebens_clean.csv")|
#    !file.exists("./data/raw_data/raw_by_country/New Zealand_Edney_Browne_2018_clean.xlsx")) {
#            source("./scripts/clean_seebens.R")
#            source("./scripts/clean_new_zealand.R")
#            }

# List all the data files
file_list <- dir(path="./data/raw_data/raw_by_country", pattern='*.xlsx')  # makes list of the files
file_listp <- paste0("./data/raw_data/raw_by_country/", file_list)         # adds path to file names


#####################################
### Making the occurrence table   ###
#####################################

# apply that function over the list of dataframes
occurr_list <- lapply(file_listp, separate_occurrence) 

# put all occurrence dataframes into one large dataframe
df_occurr <- occurr_list %>% 
             purrr::reduce(full_join) %>% 
             mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
             mutate(genus_species = gsub("\\ssp(\\.|p|\\d)$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\sn\\.sp$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\t", " ", genus_species, perl=TRUE),
                    genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
                    genus_species = gsub("[A-Z]{1}$", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\snr\\s", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\sgr\\s", " ", genus_species, perl=TRUE),
                    genus_species = gsub("\\s=(.*)", "", genus_species, perl=TRUE),
                    genus_species = gsub("\\s\\((.*)", "", genus_species, perl=TRUE),
                    genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
                    genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
                    genus_species = gsub("\\sbiotype", "", genus_species, perl=TRUE)
                    ) %>% 
             # fill in country column with canada_or_us info
             mutate(country = ifelse(is.na(country) & canada_or_us %in% c("Canada", "Us", "Us, may not actually be adventive"), 
                                     canada_or_us, country),
                    present_status = ifelse(present_status == "Na", NA, present_status),
                    notes = ifelse(country == "Us, may not actually be adventive", "may not actually be adventive", ""),
                    country = ifelse(country == "Us, may not actually be adventive", "Us", country),
                    notes = ifelse(origin == "New insect record for 1960  purposeful introduction", 
                                   "New insect record for 1960  purposeful introduction", ""),
                    origin = ifelse(origin == "New insect record for 1960  purposeful introduction",
                                    "", origin),
                    notes = ifelse(origin == "New insect record for 1963, chance immigrant", 
                                   "New insect record for 1963, chance immigrant", ""),
                    origin = ifelse(origin == "New insect record for 1963, chance immigrant",
                                    "", origin)
                    ) %>% 
             # clean up/fill in country column
             mutate(year = ifelse(year == -999, NA, year),
                    country = ifelse(region %in% c("Okinawa", "Ogasawara", "Japan"), "Japan", country),
                    country = ifelse(region == "Hawaii", "Us", country),
                    country = ifelse(region == "Korea", "Korea", country),
                    country = ifelse(region == "New Zealand", "New Zealand", country),
                    notes = ifelse(grepl("Proceedings of the", .$origin), origin, notes),
                    origin = ifelse(grepl("Proceedings of the", .$origin), "", origin)) %>% 
             # clean up origin column
             mutate(origin = gsub("&", "", origin),
                    origin = gsub("Indomaraya|indomalaya", "Indomalaya", origin),
                    origin = gsub("IndomalayaOceania", "Indomalaya, Oceania", origin),
                    origin = gsub("Middle East", "Middle_East", origin),
                    origin = gsub("cosmopolitan|Cosmoploitan", "Cosmopolitan", origin),
                    origin = gsub("S.\\sAfrica|Sth\\sAfrica", "South_Africa", origin),
                    origin = gsub("\\(Taiwan", "Taiwan", origin),
                    origin = gsub("\\(Okinawa|\\(Okinawa\\)", "Okinawa", origin),
                    origin = gsub("\\(Ogasawara", "Ogasawara", origin),
                    origin = gsub("\\(Java", "Java", origin),
                    origin = gsub("N.\\sAmerica", "North_America", origin),
                    origin = gsub("S.\\sAmerica", "South_America", origin),
                    origin = gsub("C.\\sAmerica", "Central_America", origin),
                    origin = gsub("Palearctic\\(Asia\\)|Plearctic\\(Asia\\)", "Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\s\\(Asia\\)|Paleartic\\(Asia\\)", "Palearctic_Asia", origin),
                    origin = gsub("Ppalearctic\\(Asia\\)|Palearctic\\(Asia", "Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\s\\(Asia|Paleartic\\(Asia", "Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\s\\(E.\\sAsia|Palearctic\\s\\(Central\\sAsia", "Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\(Europe\\)|Palearctic\\s\\(Europe\\)", "Palearctic_Europe", origin),
                    origin = gsub("alearctic\\(Europe\\)|Palearctic\\(Europe", "Palearctic_Europe", origin),
                    origin = gsub("Palearctic\\s\\(Europe|Paleartic\\(Europe", "Palearctic_Europe", origin),
                    origin = gsub("Parearctic\\(Europe", "Palearctic_Europe", origin),
                    origin = gsub("Palearctic\\s\\(Eurasia", "Palearctic_Europe, Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\(Europe\\)Nearctic", "Palearctic_Europe, Nearctic", origin),
                    origin = gsub("Nearctic Nearctic", "Nearctic", origin),
                    origin = gsub("Nearctic\\(Europe\\)", "Palearctic_Europe", origin),
                    origin = gsub("\\\"Old World\\\"\\/Europe", "Old_World_Europe", origin),
                    origin = gsub("Sri\\sLanka\\sor\\sAustralasia\\?\\s\\(Dugdale,\\s1988", "Sri Lanka Australasia", origin),
                    origin = gsub("C\\/C.\\sAmerica\\?\\sOld\\sworld\\stropics\\s\\(Mound\\s&\\sWalker,\\s1982", 
                                  "Cosmopolitan, Central_America, Old_World_tropics", origin),
                    origin = gsub(" ", ", ", origin),
                    origin = gsub(", , ", ", ", origin),
                    # origin = gsub(",", ", ", origin),
                    origin = gsub(",, ", ", ", origin) 
                    
                    # origin = strsplit(origin, ", ")
                    # origin = gsub("\\b", '"', origin, perl=T)
                    ) %>% 
             mutate(genus_species = gsub("Mycetophila\xa0propria", "Mycetophila propria", genus_species),
                    genus_species = gsub("Mycetophila\xa0vulgaris", "Mycetophila vulgaris", genus_species),
                    genus_species = gsub("Mycetophila\xa0marginepunctata", "Mycetophila marginepunctata", genus_species),
                    ) %>%         
             # add country codes for country and origin columns
             mutate(country_code = countrycode(country, "country.name", "iso3n", warn = TRUE),
                    origin_code = countrycode(origin, "country.name", "iso3n", warn = TRUE)) %>% 
             mutate(genus_species = gsub("\xa0", " ", genus_species , perl=TRUE)) %>% # trying to get rid of weird characters
             dplyr::select(-canada_or_us, -nz_region) %>% 
             dplyr::arrange(genus_species) 

# add the unique ID column and delete genus species column(s)
tax_table <- read.csv("./data/clean_data/taxonomy_table.csv", stringsAsFactors=F)  # read in the taxonomy table

# make final occurrence dataframe
occurr_df <- df_occurr %>%
             mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
             dplyr::rename(user_supplied_name = genus_species) %>% # have to rename genus_species to user_supplied_name so matches are correct
             dplyr::left_join(y = select(tax_table, c(user_supplied_name, taxon_id, genus_species)),
                              by = "user_supplied_name") %>% # join in the taxonomy info
             select(taxon_id, everything()) %>% # make taxon_id column the first column
             dplyr::arrange(region) # order by region


#####################################
### Write file                    ###
#####################################
# write the clean occurrence table to a CSV file
readr::write_csv(occurr_df, "./data/clean_data/occurrence_table.csv")





