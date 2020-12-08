####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### custom functions for cleaning taxonomic info             #####
##### created by Rachael Blake    04/11/2019                   #####
####################################################################

######################
#' Title: a function that cleans the dataframes and separates taxonomy columns
#'
#' @param df_location 
#'
#' @return
#' @export
#'
#' @examples
separate_taxonomy <- function(df_location){
                     # reads the excel file in
                     df <- read_excel(df_location, trim_ws = TRUE, col_types = "text")       
      
                     # clean up column names, capitalization, etc.
                     df_1 <- df %>% 
                             # replace " " and "." with "_" in column names
                             select_all(~gsub("\\s+|\\.", "_", .)) %>%  
                             select_all(tolower) %>%  # make all column names lower case
                             mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                              "\\U\\1\\L\\2", . , perl=TRUE)) %>% 
                             mutate_all(~gsub("\\.", "", . , perl=TRUE)) 
                     
                     # define what taxonomic columns might be named        
                     tax_class <- c("kingdom", "phylum", "class", "order", "family", 
                                    "genus", "species", "genus_species", "authority",
                                    "super_family", "taxonomic_authority", "taxonomy_system") 
                     
                     # split off any columns with any taxonomic column names
                     df_2 <- df_1 %>% 
                             select(one_of(tax_class)) %>% 
                             mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
                                    genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
                                    genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
                                    genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
                                    genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
                                    genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\.", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
                                    genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
                                    genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
                                    genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
                                    )
                     
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
#' Title: get only accepted taxonomic info from GBIF
#'
#' @param taxa_name 
#'
#' @return
#' @export
#'
#' @examples
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
                                           dplyr::filter(xor(any(rank %in% c("species", "subspecies", "form", "family")), 
                                                             rank == "genus")) %>% # filter rank to species if both genus and species
                                           mutate_if(is.logical, as.character) %>% 
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


##########################################################################
#' Title: function to get taxonomic info from other databases besides GBIF
#'
#' @param taxa_name 
#'
#' @return
#' @export
#'
#' @examples
get_more_info <- function(taxa_name){
                 id <- gnr_resolve(names = taxa_name, data_source_ids=c(1,2,3,4,8,12,152,168,169),
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
                                       Sys.sleep(5)  
                                       c <- tax_name(taxa_name, 
                                                     get = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), 
                                                     db = "both")
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

###########################################################
#' Title: function to read in and separate occurrence info
#'
#' @param df_location 
#'
#' @return
#' @export
#'
#' @examples
separate_occurrence <- function(df_location){
                       # reads the excel file in
                       df <- read_excel(df_location, trim_ws = TRUE, col_types = "text") 
  
                       # clean up column names, capitalization, etc.
                       df_1 <- df %>% 
                               # replace " " and "." with "_" in column names
                               select_all(~gsub("\\s+|\\.", "_", .)) %>%  
                               select_all(tolower) %>%  # make all column names lower case
                               mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                                "\\U\\1\\L\\2", . , perl=TRUE))
                       
                       # define region
                       file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
                       country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
  
  
                       df_2 <- df_1 %>% 
                               # split off any columns that are not relevant
                               select(-one_of("kingdom", "phylum", "class", "order", "family", 
                                              "genus", "species", "authority", "super_family", 
                                              "suborder", "author", "common_name", "taxonomy_system",
                                              "phagy", "host_group", "pest_type", 
                                              "jp_name", "source", "reference", "status", "synonym",
                                              "origin2", "tsn", "comment", "original_species_name",
                                              "rank", "name_changed___1_yes__0__no_", "phagy_main",
                                              "feeding_type", "feeding_main", "size_mm_", 
                                              "current_distribution_cosmopolitan_", "town", "rege_date_source",
                                              "nz_area_code", "life_form", "data_quality", "first_record_orig"
                                              )) %>% 
                               # add the name of the country as a column
                               mutate(region = country_nm) %>% 
                               mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 
                               # replace any non-numerical values in year column with NA
                               mutate(year = gsub("u", NA_character_, year, perl=TRUE)) %>% 
                               mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
                                      genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
                                      genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
                                      genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
                                      genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\.", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
                                      )
       
                       # return df_2 
                       return(df_2)
                       }
######################

############################################################
#' Title: function to separate taxa attributes info from the raw files
#'
#' @param df_location 
#'
#' @return
#' @export
#'
#' @examples
separate_attributes <- function(df_location){
                       # reads the excel file in
                       df <- read_excel(df_location, trim_ws = TRUE, col_types = "text") 
  
                       # clean up column names, capitalization, etc.
                       df_1 <- df %>% 
                               # replace " " and "." with "_" in column names
                               select_all(~gsub("\\s+|\\.", "_", .)) %>%  
                               select_all(tolower) %>%  # make all column names lower case
                               mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                                "\\U\\1\\L\\2", . , perl=TRUE))
  
                       # define region
                       file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
                       country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])

                       df_2 <- df_1 %>% 
                               # split off any columns that are not relevant
                               select(-one_of("kingdom", "phylum", "class", "order", "family", 
                                              "genus", "species", "authority", "super_family", 
                                              "suborder", "author", "common_name", "taxonomy_system",
                                              "jp_name", "source", "reference", "status", "synonym",
                                              "origin2", "tsn", "comment", "original_species_name",
                                              "rank", "name_changed___1_yes__0__no_", "size_mm_", 
                                              "town", "rege_date_source", "nz_area_code", "life_form", 
                                              "data_quality", "year", "canada_or_us"
                                              )) %>% 
                               mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 
                               # clean up the taxonomic names
                               mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
                                      genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
                                      genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
                                      genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
                                      genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\.", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
                                      genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
                                      genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
                                      ) %>% 
                               # add country_nm column
                               mutate(country_nm = country_nm)                                
                                                           
                       return(df_2)
                       }
                         
                         
#############################################################
#' Title: function to get only accepted family info from GBIF
#'
#' @param taxa_name 
#'
#' @return
#' @export
#'
#' @examples
get_accepted_families <- function(taxa_name){
                         # get taxa ids, authoritative names, and names higher up 
                         id <- get_gbifid_(taxa_name)  # gets ID from GBIF
  
                         # deal with cases where species name not found
                         if (nrow(id[[1]]) == 0) {data.frame(user_supplied_name = taxa_name,
                                                             family = "not found")
    
                         } else { 
                         xtra_cols <- c(#"rank", "status", "matchtype", "confidence", "synonym", "acceptedusagekey",
                                        "kingdomkey", "phylumkey", "classkey", "orderkey", "familykey", "note")
    
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
                                      mutate_if(is.logical, as.character) %>% 
                                      select(-one_of(xtra_cols)) 
                            
                         id_acc <- if (nrow(id_acc)>1) {id_acc[1,]} else {id_acc} # if more than one row, select first row
                             
                         # make df of all taxonomic info from GBIF
                         tax_gbif <- id_acc %>%
                                     # get authority
                                     mutate(taxonomic_authority = ifelse(sapply(strsplit(scientificname, " "), length) == 1,
                                                                  NA_character_,
                                                                  gsub("^\\w+\\s+\\w+\\s+(.*)", "\\1", scientificname))) %>%
                                     mutate(taxonomy_system = "GBIF") %>% # fill in taxonomy system source
                                     select(-scientificname, -canonicalname, -confidence) %>% 
                                     mutate_if(is.logical, as.character) 
      
                         return(tax_gbif)
                         }
                        }
                       }

                         
#######################################################
#' Title: function to manually coalesce rows in attribute table   
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
coalesce_manual <- function(df) {
                   # test whether there are multiple rows
                   if(nrow(df) == 1){coal_manual <- df %>% 
                                                    mutate_at(vars(taxon_id, genus_species, plant_feeding, 
                                                                   intentional_release, ever_introduced_anywhere,
                                                                   host_type, established_indoors_or_outdoors, host_group, 
                                                                   phagy, pest_type, ecozone, phagy_main, 
                                                                   current_distribution_cosmopolitan_, feeding_type, feeding_main,
                                                                   confirmed_establishment),
                                                              list(as.character)) %>% 
                                                    mutate_at(vars(origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
                                                                   origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
                                                                   origin_Australasia, origin_Oceania),
                                                              list(as.numeric))
                   } else {
                    # coalesce non-origin columns
                    coal_other <- df %>% 
                                  select(taxon_id, genus_species, plant_feeding, intentional_release, ever_introduced_anywhere,
                                         host_type, established_indoors_or_outdoors, host_group, phagy, pest_type, 
                                         ecozone, current_distribution_cosmopolitan_, phagy_main, feeding_type, feeding_main,
                                         confirmed_establishment) %>% 
                                  group_by(genus_species) %>%
                                  summarize_all(DescTools::Mode, na.rm = TRUE) %>% 
                                  ungroup()
                      
                    # coalesce origin columns
                    coal_origin <- df %>% 
                                   select(genus_species, starts_with("origin_")) %>% 
                                   mutate_at(vars(origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
                                                  origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
                                                  origin_Australasia, origin_Oceania),
                                             list(as.numeric)) %>% 
                                   group_by(genus_species) %>% 
                                   summarize_all( ~ ifelse((sum(., na.rm = TRUE) %in% c(1:10)), 1, 0)) %>% 
                                   ungroup()
   
                    # bind together origin and non-origin columns
                    coal_manual <- full_join(coal_other, coal_origin) %>% 
                                   select(genus_species, origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
                                          origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
                                          origin_Australasia, origin_Oceania, plant_feeding, intentional_release, 
                                          ever_introduced_anywhere, everything()) %>% 
                                   mutate_at(vars(taxon_id, genus_species, plant_feeding, 
                                                  intentional_release, ever_introduced_anywhere,
                                                  host_type, established_indoors_or_outdoors, host_group, 
                                                  phagy, pest_type, ecozone, phagy_main, 
                                                  current_distribution_cosmopolitan_, feeding_type, feeding_main,
                                                  confirmed_establishment),
                                             list(as.character)) %>% 
                                   mutate_at(vars(origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
                                                  origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
                                                  origin_Australasia, origin_Oceania),
                                             list(as.numeric))
                    }
                    
                   return(coal_manual)
                         
                   }

                         
                         
                         
                         
                         
                       
                       
                       
                       
                       
                       
                       
                       
                       
                       