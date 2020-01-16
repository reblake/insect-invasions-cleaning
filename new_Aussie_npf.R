####################
### Script to incorporate Helen's new Australian non-plant-feeding list
### Jan 16, 2020;  Script by Rachael Blake
####################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) ; library(here)

# set working directory
setwd("/nfs/insectinvasions-data")

# read in existing Australia non-plant-feeding file
aus_npf <- read_excel(here("data/raw_data/raw_by_country/Australia_NON-NATIVE_PLANT_FEEDERS_ants.xlsx"), trim_ws = TRUE, col_types = "text") 


# read in Helen's new Australian npf file
HN_nonplant <- read_excel(here("data/raw_data/allCOL_HN13Jan2020.xlsx"), trim_ws = TRUE, col_types = "text") 

new_npf <- HN_nonplant %>% 
           select(Species, Order, Family, Australia) %>% 
           filter(!is.na(Australia)) %>% 
           rename(genus_species = Species, ORDER = Order, FAMILY = Family) %>% 
           select(-Australia)
  
# combine cases
npf_combo <- full_join(aus_npf, new_npf, by = "genus_species")

# Hrm, 64 new cases, not 13 like I expected from Rebecca's e-mail
new_npf_no_match <- anti_join(new_npf, aus_npf, by = "genus_species")
nrow(new_npf_no_match)
View(new_npf_no_match)
