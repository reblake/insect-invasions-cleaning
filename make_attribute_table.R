####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean attribute table           #####
##### created by Rachael Blake    09/23/2019                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) 

# source the custom functions if they aren't in your R environment
#source("./custom_taxonomy_funcs.R")

# List all the data files
file_list <- dir(path="nfs_data/data/raw_data/raw_by_country", pattern='*.xlsx')  # makes list of the files
file_listp <- paste0("nfs_data/data/raw_data/raw_by_country/", file_list)         # adds path to file names

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

# plant feeding attribute column from the non-plant-feeding_taxa file
npf_file <- "/nfs/insectinvasions-data/data/raw_data/taxonomic_reference/non-plant-feeding_taxa_updatedOct07.xlsx"
npf_ord <- read_excel(npf_file, sheet = 2, trim_ws = TRUE, col_types = "text")
npf_fams <- read_excel(npf_file, sheet = 3, trim_ws = TRUE, col_types = "text")
npf_gen <- read_excel(npf_file, sheet = 4, trim_ws = TRUE, col_types = "text")
pf_gen <- read_excel(npf_file, sheet = 6, trim_ws = TRUE, col_types = "text")
pf_sp <- read_excel(npf_file, sheet = 7, trim_ws = TRUE, col_types = "text")

# make plant feeding taxa names title case; make vectors using dplyr::pull() 
npf_ord <- npf_ord %>% mutate(npf_orders = str_to_title(npf_orders)) %>% pull()
npf_fams <- npf_fams %>% mutate(npf_families = str_to_title(npf_families)) %>% pull()
npf_gen <- npf_gen %>% mutate(npf_genus = str_to_title(npf_genus)) %>% pull()
pf_gen <- pf_gen %>% pull()
pf_sp <- pf_sp %>% pull()

# function to collapse rows with multiple entries
coalesce_by_column <- function(df) {
                      return(dplyr::coalesce(!!! as.list(df)))
                      }

# make attribute table
df_attrib_o <- df_attrib %>% 
               left_join(tax_cols, by = c("genus_species" = "user_supplied_name")) %>% # merge in taxonomic info
               left_join(o_corr_table) %>%  # merge in origin correspondence table
               # add plant feeding attribute column
               mutate(plant_feeding = "Y",
                      plant_feeding = ifelse(order %in% npf_ord, "N", plant_feeding),
                      plant_feeding = ifelse((order == "Blattodea" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Coleoptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Hemiptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Hymenoptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Lepidoptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family == "Phlaeothripidae" & genus %in% npf_gen), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family == "Thripidae" & genus %in% npf_gen), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Coleoptera" & family == "Coccinellidae" & genus %in% pf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Muscidae" & genus %in% pf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Phoridae" & genus %in% pf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Drosophilidae" & genus_species %in% pf_sp), "Y", plant_feeding)
                      ) %>% 
               # clean up intentional release column
               mutate(intentional_release = ifelse(intentional_release %in% c("N"), "No", 
                                            ifelse(intentional_release %in% c("1", "I"), "Yes", intentional_release))) %>% 
               # add column for whether species every introduced anywhere in world
               group_by(genus_species) %>% 
               mutate(ever_introduced_anywhere = ifelse(intentional_release %in% c("Yes", "Eradicated"), "Yes", 
                                                  ifelse(intentional_release %in% c("No"), "No", NA_character_))) %>% 
               ungroup() %>% 
               # coalesce rows to one per species
               select(-origin, -country_nm, -country, -nz_region) %>% 
               group_by(genus_species) %>%
               summarise_all(coalesce_by_column) %>% 
               ungroup() %>%    
               # arrange rows and columns
               arrange(order, family, genus, genus_species) %>% 
               select(taxon_id, genus_species, plant_feeding, order, family, genus, 
                      origin_Nearctic, origin_Neotropic, origin_European_Palearctic, origin_Asian_Palearctic, origin_Indomalaya, 
                      origin_Afrotropic, origin_Australasia, origin_Oceania, everything())


#####################################
### Write file                    ###
#####################################
# write out the attribute table
readr::write_csv(df_attrib_o, "/nfs/insectinvasions-data/data/clean_data/attribute_table.csv")




	
	
