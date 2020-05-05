####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### check the Coleoptera family names                        #####
##### created by Rachael Blake    05/05/2020                   #####
####################################################################

library(tidyverse); library(readxl) ; library(taxize) 

# source the custom functions 
# source("./custom_taxonomy_funcs.R")

# read in the list of families from Sandy Liebhold
floc <- "/nfs/insectinvasions-data/data/raw_data/Copy of number of native species by beetle family in regions2.xlsx"
beetle_fams <- read_excel(floc, trim_ws = TRUE, col_types = "text" )
bfams_list <- beetle_fams$Family

# Run the bfams_list through the GBIF taxonomic cleaning process
beetle_accepted <- lapply(bfams_list, get_accepted_families)

# make dataframe
suppressMessages(
b_acc_df <- beetle_accepted %>% 
            purrr::reduce(full_join) %>% 
            select(-genus, -genuskey)
)


readr::write_csv(b_acc_df, "/nfs/insectinvasions-data/data/clean_data/Coleoptera_accepted_taxonomy.csv")

