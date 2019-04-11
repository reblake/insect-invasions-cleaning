####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### custom functions for cleaning taxonomic info             #####
##### created by Rachael Blake    04/11/2019                   #####
####################################################################

######################
# a function that cleans the dataframes and separates taxonomy columns
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


# ######################
# # a function to get raw taxonomic return info from GBIF
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
