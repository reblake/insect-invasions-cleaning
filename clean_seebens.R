####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to clean Seebens data file                #####
##### created by Rachael Blake    11/19/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl)

# set working directory
setwd("/nfs/insectinvasions-data")

# define path to Seebens raw messy file
s_path <- "./data/raw_data/raw_multiple_worksheets/Seebens_insect_first_record.xlsx"

# read in the Seebens file, only the global occurrence worksheet
seeb_df <- read_excel(s_path, sheet = "GlobalAlienSpeciesFirstRecordDa")

# clean the dataframe to merge with occurrence table 

seeb_df1 <- seeb_df %>% 
            select_all(tolower) %>%  # make all column names lower case
            dplyr::select(-origname, -author) %>%  # remove columns
            dplyr::rename(genus_species = taxon,
                          taxonomy_system = taxonomy,
                          life_form = lifeform, 
                          present_status = presentstatus,
                          first_record = firstrecord,
                          first_record_orig = firstrecord_orig,
                          data_quality = dataquality) 
         # may use this later?  but not going to spend the time right now
         # mutate(author_1 = sapply(strsplit(author, split=" ") , function(x) x[3]),
         #        author_2 = sapply(strsplit(author, split=" ") , function(x) x[4]),
         #        author = paste0(author_1, author_2)) #%>% 
         
# write the clean Seebens table to a CSV file
readr::write_csv(seeb_df1, "./data/raw_data/seebens_clean.csv")




