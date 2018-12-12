####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean occurrence table          #####
##### created by Rachael Blake    11/19/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl)

# set working directory
setwd("/nfs/insectinvasions-data")

# checks to see if clean flat files exist, otherwise creates them from multi-worksheet files
if(!file.exists("./data/raw_data/seebens_clean.csv")|
   !file.exists("./data/raw_data/raw_by_country/New Zealand_Edney_Browne_2018_clean.xlsx")) {
           source("./scripts/clean_seebens.R")
           source("./scripts/clean_new_zealand.R")
           }

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
                       
                       # define what taxonomic columns might be named        
                       tax_class <- c("kingdom", "phylum", "class", "order", "family", 
                                      "genus", "species", "genus_species", "authority",
                                      "super_family", "taxonomic_authority", "taxonomy_system") 
                       
                       # define region
                       file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
                       country <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
                         
                       # split off any columns that are _NOT_ taxonomic columns but retain genus_species
                       df_2 <- df_1 %>% 
                               select(-one_of("kingdom", "phylum", "class", "order", "family", 
                                              "authority", "super_family")) %>%   
                       # add the name of the country as a column
                               mutate(region = country)
                       
            
                       # return df_2 
                       return(df_2)
                       
                       }


# apply that function over the list of dataframes
occurr_list <- lapply(file_listp, separate_occurrence) 

# put all occurrence dataframes into one large dataframe
occurr_df <- occurr_list %>% 
             purrr::reduce(full_join) %>% 
             dplyr::select(-x__1, -x__2) %>% # remove empty columns named by Excel
             mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
             dplyr::arrange(genus_species) 

# add the unique ID column and delete genus species column(s)
tax_table <- read.csv("./data/clean_data/taxonomy_table.csv", stringsAsFactors=F)  # read in the taxonomy table

# define what taxonomic columns might be named        
tax_class <- c("kingdom", "phylum", "class", "order", "family", "super_family",
               "genus", "species", "genus_species", "taxonomic_authority", "taxonomy_system") 

# define what other columns should not be included in occurrence table
other_info <- c("phagy", "phagy_main", "feeding_type", "feeding_main", "size_mm_", "life_form")

#####################################
### Add in Seebens data           ###
#####################################
seeb_data <- read.csv("./data/raw_data/seebens_clean.csv", stringsAsFactors=F)  # read in the taxonomy table

############ filter out the New Zealand records from the Seebens file

occurr_df1 <- occurr_df %>% 
              bind_rows(seeb_data) %>%  # add in the Seebens data
              mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
              dplyr::left_join(y = select(tax_table, c(genus_species, taxon_id)), 
                               by = "genus_species") %>% # join in the taxonomy info
              select(-one_of(tax_class)) %>%  # remove the taxonomy columns but retain taxon_id
              select(-one_of(other_info)) %>% # remove the feeding habit columns
              select(taxon_id, everything()) %>% # make taxon_id column the first column
              dplyr::arrange(region) # order by region

#####################################
### Write file                    ###
#####################################
# write the clean occurrence table to a CSV file
readr::write_csv(occurr_df1, "./data/clean_data/occurrence_table.csv")





