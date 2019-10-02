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

# bring in taxonomic table for order, family, and genus columns
tax_table <- read_csv("/nfs/insectinvasions-data/data/clean_data/taxonomy_table.csv")
tax_cols <- tax_table %>% select(taxon_id, user_supplied_name, order, family, genus)

# origin_correspondence_table.xlsx for the 8 biogeographic regions
o_corr_table <- read_excel("/nfs/insectinvasions-data/data/raw_data/taxonomic_reference/origin_correspondence_table.xlsx", trim_ws = TRUE, col_types = "text") 

# plant feeding attribute column from the non-plant-feeding_taxa.csv
plf <- read_csv("/nfs/insectinvasions-data/data/raw_data/taxonomic_reference/non-plant-feeding_taxa.csv")
nplf_orders <- plf$`non-plant feeding Order`[1:16]
nplf_fams <- plf$`non plant feeding Family`[17:261]
nplf_gen <- plf$`non-plant feeding Genus`[c(253:257, 261)]
plf_gen <- plf$`plant feeding Genus`[!is.na(plf$`plant feeding Genus`)]
plf_sp <- plf$`plant feeding Species`[!is.na(plf$`plant feeding Species`)]


df_attrib_o <- df_attrib %>% 
               left_join(tax_cols, by = c("genus_species" = "user_supplied_name")) %>% # merge in taxonomic info
               left_join(o_corr_table) %>%  # merge in origin correspondence table
               # add plant feeding attribute column
               mutate(plant_feeding = "Y",
                      plant_feeding = ifelse(order %in% nplf_orders, "N", plant_feeding),
                      plant_feeding = ifelse((order == "Coleoptera" & family %in% nplf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family %in% nplf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Hemiptera" & family %in% nplf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Hymenoptera" & family %in% nplf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Blattodea" & family %in% nplf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Lepidoptera" & family %in% nplf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family %in% nplf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family == "Thripidae" & genus %in% nplf_gen), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family == "Phlaeothripidae" & genus %in% nplf_gen), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family == "Aleurodothrips" & genus %in% nplf_gen), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Coleoptera" & family == "Coccinellidae" & genus %in% plf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Muscidae" & genus %in% plf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Phoridae" & genus %in% plf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Drosophilidae" & genus_species %in% plf_sp), "Y", plant_feeding)
                      ) %>% 
               arrange(order, family, genus, genus_species) %>% 
               select(-origin) %>% 
               select(taxon_id, genus_species, plant_feeding, order, family, genus, 
                      origin_Nearctic, origin_Neotropic, origin_European_Palearctic, origin_Asian_Palearctic, origin_Indomalaya, 
                      origin_Afrotropic, origin_Australasia, origin_Oceania, everything())


#####################################
### Write file                    ###
#####################################
# write out the attribute table
readr::write_csv(df_attrib_o, "/nfs/insectinvasions-data/data/clean_data/attribute_table.csv")




	
	
	













