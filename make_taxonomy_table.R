####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean taxonomy table            #####
##### created by Rachael Blake    11/15/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(taxize) ; library(rgbif) ; library(purrr)

# source the custom functions 
source("./custom_taxonomy_funcs.R")

# set working directory
setwd("/nfs/insectinvasions-data")

# checks to see if clean flat files exist, otherwise creates them from multi-worksheet files
# if(!file.exists("./data/raw_data/seebens_clean.csv")|
#    !file.exists("./data/raw_data/raw_by_country/New Zealand_Edney_Browne_2018_clean.xlsx")) {
#            source("./scripts/clean_seebens.R")
#            source("./scripts/clean_new_zealand.R")
#            }

# List all the raw data files
file_list <- dir(path="./data/raw_data/raw_by_country", pattern='*.xlsx')  # makes list of the files
file_listp <- paste0("./data/raw_data/raw_by_country/", file_list)         # adds path to file names


####################################
### Making the taxonomic table   ###
####################################

# apply the separate_taxonomy function over the list of dataframes
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
### Make large table with all info

# also correct mis-spellings of certain species based on expert review by A. Liebhold

tax_df1 <- tax_df %>% 
           mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 
           mutate_at(.vars = vars(genus_species), .funs = list(~str_squish)) %>% 
           mutate(genus_species = if_else(genus_species == "Acronota lugens", "Acrotona lugens", genus_species),
                  genus_species = if_else(genus_species == "Continarinea pyrivora", "Contarinia pyrivora", genus_species),
                  genus_species = if_else(genus_species == "Datamicra canescens", "Datomicra canescens", genus_species),
                  genus_species = if_else(genus_species == "Datamicra celata", "Datomicra celata", genus_species),
                  genus_species = if_else(genus_species == "Datamicra zosterae", "Datomicra zosterae", genus_species),
                  genus_species = if_else(genus_species == "Evbrissa vittata", "Hemyda vittata", genus_species),
                  genus_species = if_else(genus_species == "Hypomierogaster tiro", "Hypomicrogaster tiro", genus_species),
                  genus_species = if_else(genus_species == "Oedophyrus helleri", "Oedophrys helleri", genus_species),
                  genus_species = if_else(genus_species == "Tigonotylus tenius", "Trigonotylus tenius", genus_species),
                  genus_species = if_else(genus_species == "Phyttalia（Opius） fletcheri", "Psyttalia fletcheri", genus_species),
                  genus_species = if_else(genus_species == "Polyspilla polyspilla", "Calligrapha polyspila", genus_species),
                  genus_species = if_else(genus_species == "Parasclerocoelus mediospinosa", "Limosina mediospinosa", genus_species),
                  genus_species = if_else(genus_species == "Baridinae gen", "Curculionidae", genus_species),
                  genus_species = if_else(genus_species == "Amphiareus eonstricta", "Amphiareus constrictus", genus_species),
                  genus_species = if_else(genus_species == "Brumoides ohotai", "Brumoides ohtai", genus_species),
                  genus_species = if_else(genus_species == "Caryedes serratus", "Caryedon serratus", genus_species),
                  genus_species = if_else(genus_species == "Dimetrota cursors", "Atheta cursor", genus_species),
                  genus_species = if_else(genus_species == "Allygidius modestus", "Allygus modestus", genus_species),
                  genus_species = if_else(genus_species == "Ametadoria misella", "Anisia misella", genus_species),
                  genus_species = if_else(genus_species == "Amischa curtipennis", "Quedius curtipennis", genus_species),
                  genus_species = if_else(genus_species == "Amischa filaria", "Sipalia filaria", genus_species),
                  genus_species = if_else(genus_species == "Artogeia canidia", "Pieris canidia", genus_species),
                  genus_species = if_else(genus_species == "Altermetoponia rubriceps", "Inopus rubiceps", genus_species),
                  genus_species = if_else(genus_species == "Helcystogramma sp nr phryganitis", "Helcystogramma sp", genus_species),
                  genus_species = if_else(genus_species == "Laspeyresia pomonella", "Cydia pomonella", genus_species),
                  genus_species = if_else(genus_species == "Labia anmdata", "Labia minor", genus_species),
                  genus_species = if_else(genus_species == "Apanteles melanoscelus", "Cotesia melanoscelus", genus_species),
                  genus_species = if_else(genus_species == "Biosteres oophilus oophilus", "Biosteres oophilus", genus_species),
                  genus_species = if_else(genus_species == "Hylephila silvestris", "Thymelicus sylvestris", genus_species),
                  genus_species = if_else(genus_species == "Pentaloma nigra", "Pentatoma nigra", genus_species)
                  ) %>% 
           distinct(genus_species) %>%  # remove species duplicates          
           dplyr::arrange(genus_species) # arrange alphabetically
  

#####################################
### Make vectors of genus names (no species info) and species names
# make character vector of names only to genus
g_sp <- grep('\\<sp\\>', tax_df1$genus_species, value=TRUE) 
g_spp <- grep('\\<sp.\\>', tax_df1$genus_species, value=TRUE)
bard <- as.character(dplyr::filter(tax_df1, genus_species == "Curculionidae")) # include family here
tax_vec_gn <- c(g_sp, g_spp, bard) %>% gsub(" [a-zA-Z0-9]*", "", .) %>% 
              magrittr::extract(!(. == "Tasconotus")) # remove this species

# makes character vector of names only to species 
tax_vec_sp <- unlist(tax_df1$genus_species, use.names = FALSE) %>% 
              magrittr::extract(!(. %in% g_sp)) %>% 
              magrittr::extract(!(. %in% g_spp)) %>% 
              magrittr::extract(!(. == "Curculionidae")) # this family put with genus above


#####################################
### Get taxonomy info from GBIF   ###
#####################################
xtra_cols <- c("kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
               "note", "familykey", "genuskey", "scientificname", "canonicalname", "confidence")

######################
# apply the get_accepted_taxonomy function over the vector of species names
tax_acc_l <- lapply(tax_vec_sp, get_accepted_taxonomy) 


# make dataframe of all results
suppressMessages(
tax_acc <- tax_acc_l %>% 
           purrr::reduce(full_join) %>% 
           mutate(genus_species = str_squish(genus_species)) %>% 
           select(-one_of(xtra_cols))
)

######################
# apply the get_accepted_taxonomy function over the vector of genus names
gn_acc_l <- lapply(tax_vec_gn, get_accepted_taxonomy) 


# make dataframe of all results
suppressMessages(
gen_acc <- gn_acc_l %>% 
           purrr::reduce(full_join) %>% 
           mutate(genus_species = str_squish(genus_species)) %>% 
           select(-one_of(xtra_cols))
)


######################
# resolve species without accepted species names

########
# genus level matches from get_accepted_taxonomy results
genus_only <- tax_acc %>% 
              dplyr::filter(rank == "genus") #%>% 
              # filter out those where user_supplied_name was only genus to begin with
              #dplyr::filter(!word(user_supplied_name,-1) == "sp")

go_vec <- unlist(genus_only$user_supplied_name, use.names = FALSE)

# apply the function over the vector of species names
tax_go_l <- lapply(go_vec, get_more_info) 

# make dataframe of all species rank matches
suppressMessages(
tax_go <- tax_go_l %>% 
          purrr::reduce(full_join) %>% # join all data frames from list
          dplyr::filter(!(matched_name2 == "species not found")) %>% # remove taxa that didn't provide a species-level match (no new info)
          dplyr::filter((str_count(matched_name2, '\\s+')+1) %in% c(2,3)) %>% 
          mutate(genus = ifelse((str_count(matched_name2, '\\s+')+1) == 1, matched_name2, stringr::word(matched_name2, 1)),
                 species = ifelse((str_count(matched_name2, '\\s+')+1) %in% c(2,3), matched_name2, NA_character_),
                 genus_species = ifelse(is.na(species), paste(genus, "sp"), species)) %>% 
          select(-matched_name2)
)
                 
# How many did not return lower rank? 
suppressMessages(
no_lower <- tax_go_l %>% 
            purrr::reduce(full_join) %>% # join all data frames from list
            # filter to taxa that only returned genus (no new info)
            dplyr::filter((str_count(matched_name2, '\\s+')+1) == 1|
                           matched_name2 == "species not found") 
)

# from no_lower, the one that was not found
no_lower_not_found <- no_lower %>% filter(matched_name2 == "species not found")

# from no_lower, the genus_only matches
no_lower_genus <- no_lower %>% filter(!(matched_name2 == "species not found"))

########
# species not found at all from get_accepted_taxonomy results
not_found <- tax_acc %>% 
             dplyr::filter(genus_species == "species not found",
                           !(is.na(user_supplied_name))) %>% 
             bind_rows(no_lower_not_found)

not_found_vec <- unlist(not_found$user_supplied_name, use.names = FALSE)

# apply the function over the vector of species names
tax_nf_l <- lapply(not_found_vec, get_more_info) 

# make dataframe of matches at genus and species rank
suppressMessages(
tax_nf <- tax_nf_l %>% 
          purrr::reduce(full_join) %>% 
          dplyr::filter(!(matched_name2 == "species not found")) %>% 
          mutate(genus = ifelse((str_count(matched_name2, '\\s+')+1) == 1, matched_name2, NA_character_),
                 species = ifelse((str_count(matched_name2, '\\s+')+1) %in% c(2,3), matched_name2, NA_character_),
                 genus_species = ifelse(is.na(species), genus, species)) %>% 
          mutate(genus = ifelse(is.na(genus), stringr::word(species, 1), genus)) %>% 
          select(-matched_name2)
)

# How many were not found? 
suppressMessages(
no_match <- tax_nf_l %>% 
            purrr::reduce(full_join) %>% # join all data frames from list
            dplyr::filter(matched_name2 == "species not found")
)

# How many returned at genus level rank?  
suppressMessages(
nf_go <- tax_nf_l %>% 
         purrr::reduce(full_join) %>%  # join all data frames from list
         dplyr::filter((str_count(matched_name2, '\\s+')+1) == 1)
) 

########
# put together genus-level only matches
genus_matches <- no_lower_genus %>% 
                 bind_rows(nf_go) %>% 
                 mutate(genus = matched_name2) %>% 
                 dplyr::rename(genus_species = matched_name2)

# bring in hand corrections from A. Liebhold's research
sal_taxa <- read_csv("./data/raw_data/taxonomic_reference/genus_only_resolution_FIXED.csv", trim_ws = TRUE)

# add A. Liebhold's research to correct genus-level only matches
genus_match_SAL <- genus_matches %>% 
                   left_join(sal_taxa, by = "user_supplied_name") %>% 
                   transmute(user_supplied_name, 
                             taxonomy_system = ifelse(!(is.na(genus_species.y)), taxonomy_system.y, taxonomy_system.x),
                             kingdom, phylum, class, 
                             order = ifelse(!(is.na(genus_species.y)), order.y, order.x),
                             family = ifelse(!(is.na(genus_species.y)), family.y, family.x),
                             genus = ifelse(!(is.na(genus_species.y)), word(genus_species.y, 1), genus),
                             species = ifelse(!(is.na(genus_species.y)), genus_species.y, NA_character_),
                             genus_species = ifelse(!(is.na(genus_species.y)), genus_species.y, genus_species.x),
                             synonym, uid)

still_genus <- genus_match_SAL %>% 
               dplyr::filter((str_count(genus_species, '\\s+')+1) == 1)

sp_match <- genus_match_SAL %>% 
            dplyr::filter(!(str_count(genus_species, '\\s+')+1) == 1) %>% 
            mutate_if(is.logical, as.character)

dif <- setdiff(sal_taxa$genus_species, sp_match$genus_species) # taxa resolved in sal_taxa but not matched in sp_match

########


########
# put together dataframes with new info from get_new_info function

new_info <- tax_nf %>% 
            dplyr::filter((str_count(genus_species, '\\s+')+1) == 2) %>% 
            full_join(tax_go) %>% 
            full_join(sp_match) %>%  # df of corrections to species from SAL
            full_join(gen_acc) %>%  # df of taxa where user supplied name was genus only to start with
            mutate(genus = ifelse(is.na(genus), word(genus_species, 1), genus)) 




#######################################################################
### Add unique IDs and combine species list and GBIF accepted names ###
#######################################################################
tax_final <- tax_acc %>%   
             left_join(new_info, by = c("user_supplied_name")) %>%  # bind in the taxonomic names 
             transmute(user_supplied_name, uid, 
                       rank = ifelse(is.na(rank.y), rank.x, rank.y),
                       status = ifelse(is.na(status.y), status.x, status.y),
                       matchtype = ifelse(is.na(matchtype.y), matchtype.x, matchtype.y),
                       usagekey = ifelse(is.na(usagekey.y), usagekey.x, usagekey.y),
                       synonym = ifelse(is.na(synonym.y), synonym.x, synonym.y),
                       acceptedusagekey = ifelse(is.na(acceptedusagekey.y), acceptedusagekey.x, acceptedusagekey.y),
                       kingdom = ifelse(is.na(kingdom.y), kingdom.x, kingdom.y),
                       phylum = ifelse(is.na(phylum.y), phylum.x, phylum.y), 
                       class = ifelse(is.na(class.y), class.x, class.y), 
                       order = ifelse(is.na(order.y), order.x, order.y), 
                       family = ifelse(is.na(family.y), family.x, family.y), 
                       genus = ifelse(is.na(genus.y), genus.x, genus.y),
                       species = ifelse(is.na(species.y), species.x, species.y),
                       genus_species = ifelse(is.na(genus_species.y), genus_species.x, genus_species.y),
                       taxonomy_system = ifelse(is.na(taxonomy_system.y), taxonomy_system.x, taxonomy_system.y),
                       taxonomic_authority = ifelse(is.na(taxonomic_authority.y), taxonomic_authority.x, taxonomic_authority.y)) %>% 
             dplyr::filter(!(is.na(user_supplied_name))) %>% # remove blank rows
             # add the unique ID column after all unique species are in one dataframe
             tibble::rowid_to_column("taxon_id")



#####################################
### Write file                    ###
#####################################
# write the clean taxonomy table to a CSV file
readr::write_csv(tax_final, "./data/clean_data/taxonomy_table.csv")





