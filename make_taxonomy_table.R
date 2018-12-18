####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean taxonomy table            #####
##### created by Rachael Blake    11/15/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(taxize) 

# set working directory
setwd("/nfs/insectinvasions-data")

# checks to see if clean flat files exist, otherwise creates them from multi-worksheet files
if(!file.exists("./data/raw_data/seebens_clean.csv")|
   !file.exists("./data/raw_data/raw_by_country/New Zealand_Edney_Browne_2018_clean.xlsx")) {
           source("./scripts/clean_seebens.R")
           source("./scripts/clean_new_zealand.R")
           }

# List all the raw data files
file_list <- dir(path="./data/raw_data/raw_by_country", pattern='*.xlsx')  # makes list of the files
file_listp <- paste0("./data/raw_data/raw_by_country/", file_list)         # adds path to file names


####################################
### Making the taxonomic table   ###
####################################
# make a function that cleans the dataframes and separates taxonomy columns
separate_taxonomy <- function(df_location){
                     # reads the excel file in
                     df <- read_excel(df_location, trim_ws = TRUE)       
      
                     # clean up column names, capitalization, etc.
                     df_1 <- df %>% 
                             # replace " " and "." with "_" in column names
                             select_all(~gsub("\\s+|\\.", "_", .)) %>%  
                             select_all(tolower) %>%  # make all column names lower case
                             mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                              "\\U\\1\\L\\2", . , perl=TRUE))
                     
                     # define what taxonomic columns might be named        
                     tax_class <- c("kingdom", "phylum", "class", "order", "family", 
                                    "genus", "species", "genus_species", "authority",
                                    "super_family", "taxonomic_authority", "taxonomy_system") 
                     
                     # split off any columns with any taxonomic column names
                     df_2 <- df_1 %>% 
                             select(one_of(tax_class))
                     
                     # return df_2 
                     return(df_2)       
                     }


# apply that function over the list of dataframes
tax_list <- lapply(file_listp, separate_taxonomy) 

# put all taxonomy dataframes into one large dataframe
tax_df <- tax_list %>% 
          purrr::reduce(full_join) %>%  
          mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 
          dplyr::rename(taxonomic_authority = authority) %>% 
          dplyr::arrange(genus_species) 

# define what taxonomic columns might be named        
tax_class <- c("kingdom", "phylum", "class", "order", "family", "super_family",
               "genus", "species", "genus_species", "taxonomic_authority", "taxonomy_system") 


#####################################
### Add in Seebens data          
# read in the taxonomy table
seeb_data <- read.csv("./data/raw_data/seebens_clean.csv", stringsAsFactors=F)  

seeb_data1 <- seeb_data %>% 
              select(one_of(tax_class)) %>% 
              unique()

#####################################
### Make large table with all info

tax_df1 <- tax_df %>% 
           bind_rows(seeb_data1) %>% 
           mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 

           # fill in NAs if genus_species is duplicated
           group_by(genus_species) %>% 
           mutate(class = recode(class, "Hexapoda" = "Insecta")) %>% # replaces old class name with new
           fill(everything()) %>% 
           fill(everything(), .direction = "up") %>% 
           ungroup() %>%   
  
           distinct(genus_species, .keep_all = TRUE) %>%  # remove species duplicates 
           select(genus_species, everything()) %>%  # reorder columns
           dplyr::arrange(genus_species) # arrange alphabetically
  

#####################################
### Get taxonomy info from GBIF   ###
#####################################

# makes character vector of species names
tax_vec <- unlist(tax_df1$genus_species, use.names = FALSE)  

# write function(s) to apply over this character vector

get_accepted_taxonomy <- function(taxa_name){
                         # get taxa ids, authoritative names, and names higher up 
                         id <- get_gbifid_(taxa_name)  # gets ID from GBIF
                         
                         # deal with cases where species name not found
                         if (nrow(id[[1]]) == 0){data.frame(user_supplied_name = taxa_name,
                                                            genus_species = "species not found")
                             } else { 
                             xtra_cols <- c("rank", "status", "matchtype", "canonicalname", "confidence", "synonym",
                                             "kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
                                             "note", "familykey", "genuskey", "acceptedusagekey")
                               
                             # puts ID info into one dataframe
                             tax_id <- map_df(id, ~as.data.frame(.x), .id="user_supplied_name")
                             id_acc <- tax_id %>%
                                       mutate_if(is.logical, as.character) %>% 
                                       dplyr::filter(if(!(status %in% c("ACCEPTED"))) {row_number() == 1} else { 
                                                        status %in% c("ACCEPTED")}) %>%   # filter to accepted names only
                                       dplyr::filter(xor(any(rank == "species"), rank == "genus")) %>% # filter rank to species if both genus and species
                                       select(-one_of(xtra_cols)) 
 
                             # make df of all taxonomic info from GBIF
                             tax_gbif <- id_acc %>%
                                         # get authority
                                         mutate(taxonomic_authority = ifelse(!exists("species") | 
                                                                                sapply(strsplit(scientificname, " "), length) == 1,
                                                                             NA_character_,
                                                                             gsub("^\\w+\\s+\\w+\\s+(.*)", "\\1", scientificname))) %>%
                                         # get genus_species
                                         mutate(genus_species = ifelse(!exists("species"), paste(genus, "sp"), 
                                                                       gsub("^((\\w+\\W+){0,1}\\w+).*", "\\1", scientificname)))  %>%
                                         mutate(taxonomy_system = "GBIF") %>%
                                         select(-scientificname) 

                             return(tax_gbif)
                             }
                         }
#####	
# need to write if else statement for cases when only genus is found for splitting out the authority


# apply the function over the vector of species names
tax_acc_l <- lapply(tax_vec, get_accepted_taxonomy) 

# make dataframe of all results
suppressMessages(
tax_acc <- tax_acc_l %>% 
           purrr::reduce(full_join) %>% 
           filter(kingdom == "Animalia"  | is.na(kingdom), # filter to only kingdom Animalia
                  phylum == "Arthropoda" | is.na(phylum),  # filter to phylum Arthropoda only
                  class == "Insecta" | is.na(class))  # filter to only class Insecta
)


#######################################################################
### Add unique IDs and combine species list and GBIF accepted names ###
#######################################################################
tax_df_min <- tax_df1 %>% 
              # rename the genus_species column in tax_df1 to user_supplied_name
              dplyr::rename(user_supplied_name = genus_species) %>% 
              dplyr::select(user_supplied_name, super_family) # remove all columns except taxa list

tax_final <- tax_acc %>%   
             left_join(tax_df_min) %>%  # bind in the taxonomic names 
             # add the unique ID column after all unique species are in one dataframe
             tibble::rowid_to_column("taxon_id")


#####################################
### Write file                    ###
#####################################
# write the clean taxonomy table to a CSV file
readr::write_csv(tax_final, "./data/clean_data/taxonomy_table.csv")





