####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean occurrence table          #####
##### created by Rachael Blake    11/19/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) ; library(countrycode)

# source the custom functions if they aren't in your R environment
#source("nfs_data/custom_taxonomy_funcs.R")


# # checks to see if clean flat files exist, otherwise creates them from multi-worksheet files
# if(!file.exists("nfs_data/data/raw_data/seebens_clean.csv")|
#    !file.exists("nfs_data/data/raw_data/raw_by_country/New Zealand_Edney_Browne_2018_clean.xlsx")) {
#            source("nfs_data/scripts/clean_seebens.R")
#            source("nfs_data/scripts/clean_new_zealand.R")
#            }

# List all the data files
file_list <- dir(path="nfs_data/data/raw_data/raw_by_country", pattern='*.xlsx')  # makes list of the files
file_listp <- paste0("nfs_data/data/raw_data/raw_by_country/", file_list)         # adds path to file names


#####################################
### Making the occurrence table   ###
#####################################

# apply that function over the list of dataframes
occurr_list <- lapply(file_listp, separate_occurrence) 

# put all occurrence dataframes into one large dataframe
df_occurr <- occurr_list %>% 
             purrr::reduce(full_join) %>% 
             mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
             # remove Arachnid
             filter(!(genus_species == "Trixacarus caviae")) %>% 
             # add blank country and present_status columns because they were removed in edits of the raw data files (Aug 14, 2020)
             mutate(country = '',
                    present_status = '') %>% 
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
             # clean up some species names
             mutate(genus_species = gsub("Mycetophila\xa0propria", "Mycetophila propria", genus_species),
                    genus_species = gsub("Mycetophila\xa0vulgaris", "Mycetophila vulgaris", genus_species),
                    genus_species = gsub("Mycetophila\xa0marginepunctata", "Mycetophila marginepunctata", genus_species),
                    ) %>%         
             # clean up year column
             mutate(year = ifelse(year == "N/A", NA_character_, year),
                    year = gsub("\\s", "", year, perl=TRUE)) %>% 
             # clean up intentional release column
             mutate(intentional_release = ifelse(intentional_release %in% c("N"), "No", 
                                          ifelse(intentional_release %in% c("1", "I"), "Yes", intentional_release))) %>% 
             # add country codes for country and origin columns
             mutate(country_code = countrycode(country, "country.name", "iso3n", warn = TRUE),
                    origin_code = countrycode(origin, "country.name", "iso3n", warn = TRUE)) %>% 
             mutate(genus_species = gsub("\xa0", " ", genus_species , perl=TRUE)) %>% # trying to get rid of weird characters
             dplyr::select(-canada_or_us, -nz_region) %>% 
             dplyr::arrange(genus_species) 

# add the unique ID column and delete genus species column(s)
tax_table <- read.csv("nfs_data/data/clean_data/taxonomy_table.csv", stringsAsFactors=F)  # read in the taxonomy table

# make final occurrence dataframe
occurr_df <- df_occurr %>%
             mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>%  # remove rogue white spaces
             dplyr::rename(user_supplied_name = genus_species) %>% # have to rename genus_species to user_supplied_name so matches are correct
             dplyr::left_join(y = select(tax_table, c(user_supplied_name, taxon_id, genus_species)),
                              by = "user_supplied_name") %>% # join in the taxonomy info
             mutate(genus_species = gsub("<a0>", " ", genus_species, perl=TRUE)) %>% 
             select(taxon_id, everything()) %>% # make taxon_id column the first column
             dplyr::arrange(taxon_id) # order by taxon_id


#####################################
### Write file                    ###
#####################################
# write the clean occurrence table to a CSV file
readr::write_csv(occurr_df, "nfs_data/data/clean_data/occurrence_table.csv")





