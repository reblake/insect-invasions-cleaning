####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean attribute table           #####
##### created by Rachael Blake    09/23/2019                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) 

# source the custom functions 
#source("./custom_taxonomy_funcs.R")

# set working directory
setwd("/nfs/insectinvasions-data")

# List all the data files
file_list <- dir(path="./data/raw_data/raw_by_country", pattern='*.xlsx')  # makes list of the files
file_listp <- paste0("./data/raw_data/raw_by_country/", file_list)         # adds path to file names

#####################################
### Making the attribute table    ###
#####################################

# apply that function over the list of dataframes
attrib_list <- lapply(file_listp, separate_attributes) 

df_attrib <- attrib_list %>% 
             purrr::reduce(full_join) %>% 
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
                    origin = gsub(",, ", ", ", origin) 
                    ) 


### merge in origin_correspondence_table.xlsx for the 8 biogeographic regions

### bring in the plant feeding attribute column from the non-plant-feeding_taxa.csv


