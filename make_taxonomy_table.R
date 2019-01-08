####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean taxonomy table            #####
##### created by Rachael Blake    11/15/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(taxize) ; library(rgbif) ; library(purrr)

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

           # # fill in NAs if genus_species is duplicated
           # group_by(genus_species) %>% 
           # mutate(class = recode(class, "Hexapoda" = "Insecta")) %>% # replaces old class name with new
           # fill(everything()) %>% 
           # fill(everything(), .direction = "up") %>% 
           # ungroup() %>%   
           # 
           #distinct(genus_species, .keep_all = TRUE) %>%  # remove species duplicates 
           distinct(genus_species) %>%  # remove species duplicates          
           #select(genus_species, everything()) %>%  # reorder columns
           dplyr::arrange(genus_species) # arrange alphabetically
  

#####################################
### Get taxonomy info from GBIF   ###
#####################################

# makes character vector of species names
tax_vec <- unlist(tax_df1$genus_species, use.names = FALSE)  

# write function(s) to apply over this character vector

######################
# get raw taxonomic info from GBIF
get_raw_taxonomy <- function(taxa_name){
                    id <- get_gbifid_(taxa_name)  # gets ID from GBIF
                    
                    # deal with cases where species name not found
                    if (nrow(id[[1]]) == 0){data.frame(user_supplied_name = taxa_name,
                                                       genus_species = "species not found")
                       } else { 
                       # puts ID info into one dataframe
                       tax_id <- map_df(id, ~as.data.frame(.x), .id="user_supplied_name")
                       }
                       
                    return(tax_id)
                    }

# apply the function over the vector of species names
tax_raw_l <- lapply(tax_vec, get_raw_taxonomy) 

# make dataframe of all results
suppressMessages(
tax_raw <- tax_raw_l %>% 
           purrr::reduce(full_join) %>% 
           filter(kingdom == "Animalia"  | is.na(kingdom), # filter to only kingdom Animalia
                  phylum == "Arthropoda" | is.na(phylum),  # filter to phylum Arthropoda only
                  class == "Insecta" | is.na(class))  # filter to only class Insecta
)

# write the raw taxonomy table to a CSV file
#readr::write_csv(tax_raw, "./data/clean_data/taxonomy_raw.csv")
######################

######################
# get only accepted taxonomic info from GBIF
get_accepted_taxonomy <- function(taxa_name){
                         # get taxa ids, authoritative names, and names higher up 
                         id <- get_gbifid_(taxa_name)  # gets ID from GBIF
                         
                         # deal with cases where species name not found
                         if (nrow(id[[1]]) == 0) {data.frame(user_supplied_name = taxa_name,
                                                             genus_species = "species not found")
                             
                             } else { 
                             xtra_cols <- c(#"rank", "status", "matchtype", "confidence", "synonym",
                                             "kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
                                             "note", "familykey", "genuskey", "acceptedusagekey")
                               
                             # puts ID info into one dataframe
                             tax_id <- map_df(id, ~as.data.frame(.x), .id="user_supplied_name")
                             # filter dataframe for accepted names
                             id_acc <- tax_id %>% 
                                       mutate_if(is.logical, as.character) %>% 
                                       dplyr::filter(if(!(status %in% c("ACCEPTED"))) {row_number() == 1} else { 
                                                        status %in% c("ACCEPTED")}) %>%   # filter to accepted names only
                                       dplyr::filter(if(!(matchtype %in% c("EXACT", "HIGHERRANK"))) {row_number() == 1} else {
                                                        matchtype %in% c("EXACT", "HIGHERRANK")}) %>% # filter for exact matches 
                                       dplyr::filter(xor(any(rank == "species"), rank == "genus")) %>% # filter rank to species if both genus and species
                                       select(-one_of(xtra_cols)) 
 
                           
                             # make df of all taxonomic info from GBIF
                             tax_gbif <- id_acc %>%
                                         # get authority
                                         mutate(taxonomic_authority = ifelse(sapply(strsplit(scientificname, " "), length) == 1,
                                                                             NA_character_,
                                                                             gsub("^\\w+\\s+\\w+\\s+(.*)", "\\1", scientificname))) %>%
                                         mutate(taxonomic_authority = ifelse(genus %in% sapply(strsplit(scientificname, " "), unlist),
                                                                             stringr::word(taxonomic_authority,-2,-1),
                                                                             taxonomic_authority)) %>% 
                                         # get genus_species
                                         mutate(genus_species = ifelse(!exists("species"), paste(genus, "sp"), canonicalname))  %>%
                                         # filter to kingdom, phylum, class
                                         dplyr::filter(kingdom == "Animalia"  | is.na(kingdom)) %>%  
                                         dplyr::filter(if(!("phylum" %in% names(tax_id))) {TRUE} else {
                                                          phylum == "Arthropoda" | is.na(phylum)}) %>%  
                                         dplyr::filter(if(!("class" %in% names(tax_id))) {TRUE} else {
                                                          class == "Insecta"  | is.na(class)}) %>% 
                                         mutate(taxonomy_system = "GBIF") %>%
                                         select(#-rank, -status, -matchtype, -synonym,
                                                -scientificname, -canonicalname, -confidence) %>% 
                                         mutate_if(is.logical, as.character) 
                                 
                             return(tax_gbif)
                             }
                         }


# apply the function over the vector of species names
tax_acc_l <- lapply(tax_vec, get_accepted_taxonomy) 

# make dataframe of all results
suppressMessages(
tax_acc <- tax_acc_l %>% 
           purrr::reduce(full_join) 
)

######################
# resolve species without accepted species names

#####            
genus_only <- tax_acc %>% dplyr::filter(rank == "genus")

genus_only_l <- list(tax_acc$user_supplied_name)

get_lowerrank <- function(taxa_name){
                 id <- #gnr_resolve(names = taxa_name)  # no new info.
                       
                 }

                       #get_uid(sciname = taxa_name) # returns nothing
                       #get_tsn(searchterm = taxa_name, searchtype = "scientific") # returns nothing
                       #get_ids(names=taxa_name, db = "gbif")  # returns nothing
                       #gnr_resolve(names = taxa_name)   # returns nothing
                        
                 

#####
not_found <- tax_acc %>% dplyr::filter(genus_species == "species not found") %>% select(user_supplied_name)

get_not_found <- function(taxa_name){
                 id <- name_backbone(taxa_name, class="Insecta", verbose=TRUE)[[1]]  # returns something  
                   
                 # adds user_supplied_name
                 tax_id <- id %>% 
                           mutate(user_supplied_name = taxa_name) %>% 
                           select(-confidence, -scientificName, -canonicalName, 
                                  -kingdomKey, -phylumKey, -classKey) %>% 
                           select(user_supplied_name, everything())
                 
                 return(tax_id)
                 }

# apply the function over the vector of species names
tax_nf_l <- lapply(not_found, get_not_found) 

# make dataframe of all results
suppressMessages(
tax_nf <- tax_nf_l %>% 
          purrr::reduce(full_join) 
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





