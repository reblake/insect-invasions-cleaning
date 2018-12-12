####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to clean a multi-worksheet data file      #####
##### created by Rachael Blake    11/20/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(writexl)

# set working directory
setwd("/nfs/insectinvasions-data")

# define path to raw messy file
nz_path <- "./data/raw_data/raw_multiple_worksheets/New Zealand_established_Edney_Browne_etal_2018.xlsx"

# read in the file, only the master worksheet
nz_master <- read_excel(nz_path, sheet = "Master List")


# clean the dataframe to merge with occurrence table 

nz_df <- nz_master %>% 
         select_all(~gsub("\\s+|\\.|\\(|\\)", "_", .)) %>%  # replace " " and "." with "_" in column names
         select_all(~gsub("\\?", "", .)) %>%  # remove "?" in column names
         select_all(tolower) %>%  # make all column names lower case
         # combine genus and species into one column to match other data
         mutate(genus_species = paste(genus, species)) %>% 
         select(genus_species, everything()) %>%  # make genus_species column the first column
         dplyr::arrange(genus_species) 


# write the clean dataframe to a .xlsx file so it can be batch read with the other files
write_xlsx(nz_df, "./data/raw_data/raw_by_country/New Zealand_Edney_Browne_2018_clean.xlsx")



