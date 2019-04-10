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
                                              "\\U\\1\\L\\2", . , perl=TRUE)) %>% 
                             mutate_all(~gsub("\\.", "", .))
                     
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
                  genus_species = if_else(genus_species == "Baridinae gen", "Baridinae", genus_species),
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
bard <- as.character(dplyr::filter(tax_df1, genus_species == "Baridinae")) # include subfamily here
tax_vec_gn <- c(g_sp, g_spp, bard) %>% gsub(" [a-zA-Z0-9]*", "", .) %>% 
              magrittr::extract(!(. == "Tasconotus")) # remove this species

# makes character vector of names only to species 
tax_vec_sp <- unlist(tax_df1$genus_species, use.names = FALSE) %>% 
              magrittr::extract(!(. %in% g_sp)) %>% 
              magrittr::extract(!(. %in% g_spp)) %>% 
              magrittr::extract(!(. == "Baridinae")) # this subfamily put with genus above


#####################################
### Get taxonomy info from GBIF   ###
#####################################


# ######################
# # get raw taxonomic info from GBIF
# get_raw_taxonomy <- function(taxa_name){
#                     id <- get_gbifid_(taxa_name)  # gets ID from GBIF
#                     
#                     # deal with cases where species name not found
#                     if (nrow(id[[1]]) == 0){data.frame(user_supplied_name = taxa_name,
#                                                        genus_species = "species not found")
#                        } else { 
#                        # puts ID info into one dataframe
#                        tax_id <- map_df(id, ~as.data.frame(.x), .id="user_supplied_name")
#                        }
#                        
#                     return(tax_id)
#                     }
# 
# # apply the function over the vector of species names
# tax_raw_l <- lapply(tax_vec, get_raw_taxonomy) 
# 
# # make dataframe of all results
# suppressMessages(
# tax_raw <- tax_raw_l %>% 
#            purrr::reduce(full_join) %>% 
#            filter(kingdom == "Animalia"  | is.na(kingdom), # filter to only kingdom Animalia
#                   phylum == "Arthropoda" | is.na(phylum),  # filter to phylum Arthropoda only
#                   class == "Insecta" | is.na(class))  # filter to only class Insecta
# )

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
                             xtra_cols <- c(#"rank", "status", "matchtype", "confidence", "synonym", "acceptedusagekey",
                                             "kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
                                             "note", "familykey", "genuskey")
                               
                             # puts ID info into one dataframe
                             tax_id <- map_df(id, ~as.data.frame(.x), .id="user_supplied_name")
                             
                             id_insect <- tax_id %>% 
                                          mutate_if(is.logical, as.character) %>% 
                                          # filter to kingdom, phylum, class
                                          dplyr::filter(kingdom == "Animalia"  | is.na(kingdom)) %>%  
                                          dplyr::filter(if(!("phylum" %in% names(.))) {TRUE} else {
                                                        phylum == "Arthropoda" | is.na(phylum)}) %>%  
                                          dplyr::filter(if(!("class" %in% names(.))) {TRUE} else {
                                                        class == "Insecta"  | is.na(class)})
                             
                             if (nrow(id_insect) == 0) {id_insect
                                 } else {
                                 # filter dataframe for accepted names
                                 id_acc <- id_insect %>% 
                                           # filter to best matched name
                                           dplyr::filter(if (any(status %in% c("ACCEPTED") & matchtype %in% c("EXACT"))){ 
                                                             status == "ACCEPTED" & matchtype == "EXACT"
                                                         } else if (any(status %in% c("SYNONYM") & matchtype %in% c("EXACT"))) {
                                                                    status == "SYNONYM" & matchtype == "EXACT"
                                                         } else if (any(status %in% c("DOUBTFUL") & matchtype %in% c("EXACT"))) {
                                                                    status == "DOUBTFUL" & matchtype == "EXACT"
                                                         } else if (any(status %in% c("ACCEPTED") & matchtype %in% c("HIGHERRANK"))) {
                                                                    status == "ACCEPTED" & matchtype == "HIGHERRANK"  
                                                         } else if (any(status %in% c("DOUBTFUL") & matchtype %in% c("HIGHERRANK"))) {
                                                                    status == "DOUBTFUL" & matchtype == "HIGHERRANK"
                                                         } else if (any(status %in% c("ACCEPTED") & matchtype %in% c("FUZZY"))) {
                                                                    status == "ACCEPTED" & matchtype == "FUZZY"
                                                         } else if (any(status %in% c("SYNONYM") & matchtype %in% c("FUZZY"))) {
                                                                    status == "SYNONYM" & matchtype == "FUZZY" 
                                                         } else {row_number() == 1 
                                                         }) %>%  
                                           dplyr::filter(xor(any(rank %in% c("species", "subspecies", "form")), 
                                                             rank == "genus")) %>% # filter rank to species if both genus and species
                                           select(-one_of(xtra_cols)) 
                             
                                 id_acc <- if (nrow(id_acc)>1) {id_acc[1,]} else {id_acc} # if more than one row, select first row
 
                                 # make df of all taxonomic info from GBIF
                                 tax_gbif <- id_acc %>%
                                             # get authority
                                             mutate(taxonomic_authority = ifelse(sapply(strsplit(scientificname, " "), length) == 1,
                                                                                 NA_character_,
                                                                                 gsub("^\\w+\\s+\\w+\\s+(.*)", "\\1", scientificname))) %>%
                                             mutate(taxonomic_authority = ifelse(genus %in% sapply(strsplit(taxonomic_authority, " "), unlist)|
                                                                                 user_supplied_name %in% sapply(strsplit(taxonomic_authority, " "), unlist),
                                                                                 stringr::word(taxonomic_authority,-2,-1),
                                                                                 taxonomic_authority)) %>% 
                                             # get genus_species
                                             mutate(genus_species = ifelse(!exists("species")|is.na("species"), 
                                                                           paste(genus, "sp"), species))  %>%
                                             mutate(taxonomy_system = "GBIF") %>% # fill in taxonomy system source
                                             select(-scientificname, -canonicalname, -confidence) %>% 
                                             mutate_if(is.logical, as.character) 
                                     
                                 return(tax_gbif)
                                 }
                             }
                         }

######################
xtra_cols <- c("kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
               "note", "familykey", "genuskey", "scientificname", "canonicalname", "confidence")

######################
# apply the function over the vector of species names
tax_acc_l <- lapply(tax_vec_sp, get_accepted_taxonomy) 


# make dataframe of all results
suppressMessages(
tax_acc <- tax_acc_l %>% 
           purrr::reduce(full_join) %>% 
           mutate(genus_species = str_squish(genus_species)) %>% 
           select(-one_of(xtra_cols))
)

######################
# apply the function over the vector of genus names
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

######################
get_more_info <- function(taxa_name){
                 id <- gnr_resolve(names = taxa_name, data_source_ids=c(1,2,3,4,8,12,152),
                                   canonical=TRUE, best_match_only=TRUE)  
                 
                 # deal with cases where species name not found
                 id_res <- if (nrow(id) == 0) {data.frame(user_supplied_name = taxa_name,
                                                matched_name2 = "species not found")
                               } else { 
                                 tax_ids <- id %>% 
                                            dplyr::rename(taxonomy_system = data_source_title) %>% 
                                            select(-submitted_name, -score)
                           }
                 
                 if (id_res$matched_name2 == "species not found") {return(id_res)
                   
                    } else {
                      
                      # get higher taxonomic classification
                      ranks <- c("kingdom", "phylum", "class", "order", "family", "genus", "species")
                      
                      id_class <- if (!(id_res$taxonomy_system %in% c("NCBI", "ITIS"))) {id_res
                                     } else {
                                       Sys.sleep(3)  
                                       c <- tax_name(taxa_name, 
                                                     get = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), 
                                                     db = tolower(id_res$taxonomy_system))
                                     if (is.na(c$kingdom)|is.na(c$genus)|is.na(c$order)) {id_res
                                        } else {
                                          Sys.sleep(5)   
                                          uid <- get_uid_(c$species)
                                          if (is.null(uid[[1]])) {id_res
                                             } else {
                                               c3 <- bind_cols(c, data.frame(uid[[1]]$uid)) %>% 
                                                     rename(uid = uid..1...uid,
                                                            user_supplied_name = query) %>% 
                                                     select(-db)  
                                             }
                                        }
                                     }
                      
                      # merge id_res and id_class
                      id_all <- if (all(names(id_res) %in% names(id_class)) == TRUE) {id_res  
                                   } else {
                                     id_res %>% 
                                     full_join(id_class, by = c("user_supplied_name")) %>%  # bind in the taxonomic names 
                                     mutate_if(is.factor, as.character)
                                   }
                      
                      return(id_all)    
                      }
                 }
######################
########
# genus level matches from get_accepted_taxonomy
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
          mutate(genus = ifelse((str_count(matched_name2, '\\s+')+1) == 1, matched_name2, NA_character_),
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
# species not found at all from get_accepted_taxonomy
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
                 genus_species = ifelse(is.na(species), paste(genus, "sp"), species)) %>% 
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
                   # mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 
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
            dplyr::filter(!(str_count(genus_species, '\\s+')+1) == 1)

########

########
# put together dataframes with new info from get_new_info function

new_info <- tax_nf %>% 
            full_join(tax_go) %>% 
            full_join(gen_acc) %>%  # df of taxa where user supplied name was genus only to start with
            full_join(sp_match) %>%  # df of corrections to species from SAL
            mutate(genus = ifelse(is.na(genus), word(species, 1), genus)) 

# gen_acc  # genus only taxa split and run separately



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





