####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean occurrence table          #####
##### created by Rachael Blake    11/19/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr)

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

separate_occurrence <- function(df_location){
                       # reads the excel file in
                       df <- read_excel(df_location) 

                       # clean up column names, capitalization, etc.
                       df_1 <- df %>% 
                               # replace " " and "." with "_" in column names
                               select_all(~gsub("\\s+|\\.", "_", .)) %>%  
                               select_all(tolower) %>%  # make all column names lower case
                               mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                                "\\U\\1\\L\\2", . , perl=TRUE))
                      
                       # define region
                       file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
                       country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
                         
                       
                       df_2 <- df_1 %>% 
                               # split off any columns that are not relevant
                               select(-one_of("kingdom", "phylum", "class", "order", "family", 
                                              "genus", "species", "authority", "super_family", 
                                              "suborder", "author", "common_name", "taxonomy_system",
                                              "phagy", "host_group", "intentionalrelease", "pest_type",
                                              "jp_name", "source", "reference", "status", "synonym",
                                              "origin2", "tsn", "comment", "original_species_name",
                                              "rank", "name_changed___1_yes__0__no_", "phagy_main",
                                              "feeding_type", "feeding_main", "size_mm_", "dist",
                                              "current_distribution_cosmopolitan_", "town", "rege_date_source",
                                              "nz_area_code", "life_form", "data_quality", "first_record_orig",
                                              "confirmed_establishment"
                                              )) %>%   
                               # add the name of the country as a column
                               mutate(region = country_nm)
                       
                        
                       # return df_2 
                       return(df_2)
                       
                       }


# apply that function over the list of dataframes
occurr_list <- lapply(file_listp, separate_occurrence) 

# put all occurrence dataframes into one large dataframe
df_occurr <- occurr_list %>% 
             purrr::reduce(full_join) %>% 
             mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
             # filter out USA and Canada data from Seebens - keep Sandy's North America data
             dplyr::filter(!(region == "Europe" & country %in% c("Usacanada", "United States")),
                           !is.na(genus_species)) %>% 
             # fill in country column with canada_or_us info
             mutate(country = ifelse(is.na(country) & canada_or_us %in% c("Canada", "Us", "Us, may not actually be adventive"), 
                                     canada_or_us, country),
                    present_status = ifelse(present_status == "Na", NA, present_status)) %>% 
             mutate(year = ifelse(year == -999, NA, year)) %>% 
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





