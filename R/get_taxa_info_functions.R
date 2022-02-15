####################################################################
##### Functions for getting taxonomic info from GBIF etc.      #####
##### originally created by Rachael Blake    04/11/2019        #####
####################################################################

######################
#' Get accepted taxonomic information from GBIF
#'
#' If the user supplied taxa is an insect, this function will return the
#' best accepted match from GBIF.  If the user supplied taxa is not an
#' insect according to GBIF, it will return "id to non-insect species"
#' in the genus_species column.  If the user supplied taxa is not found
#' in GBIF, this function will return "species not found" in the
#' genus_species column.
#'
#' @param taxa_name list of taxa names to search for in GBIF
#'
#' @import dplyr
#' @importFrom taxize get_gbifid_
#' @importFrom purrr map_df
#' @importFrom stringr word
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' taxa_list <- c("Abax parallelopipedus", "Hypsicera curvator", "Xylocoris sordidus")
#' taxa_accepted <- lapply(taxa_list, get_accepted_taxonomy)
#' }
get_accepted_taxonomy <- function(taxa_name){
                         # get taxa ids, authoritative names, and names higher up
                         id <- taxize::get_gbifid_(taxa_name)  # gets ID from GBIF

                         # deal with cases where species name not found
                         if (nrow(id[[1]]) == 0) {data.frame(user_supplied_name = taxa_name,
                                                             genus_species = "species not found")

                             } else {
                             xtra_cols <- c(#"rank", "status", "matchtype", "confidence", "synonym", "acceptedusagekey",
                                             "kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
                                             "note", "familykey", "genuskey")

                             # puts ID info into one dataframe
                             tax_id <- purrr::map_df(id, ~as.data.frame(.x), .id="user_supplied_name")

                             id_insect <- tax_id %>%
                                          mutate_if(is.logical, as.character) %>%
                                          # filter to kingdom, phylum, class
                                          dplyr::filter(kingdom == "Animalia"  | is.na(kingdom)) %>%
                                          dplyr::filter(if(!("phylum" %in% names(.))) {TRUE} else {
                                                        phylum == "Arthropoda" | is.na(phylum)}) %>%
                                          dplyr::filter(if(!("class" %in% names(.))) {TRUE} else {
                                                        class == "Insecta"  | is.na(class)})

                             if (nrow(id_insect) == 0) {id_not_insect <- tax_id %>%
                                                                         select(user_supplied_name, rank) %>%
                                                                         mutate(rank = "unknown",
                                                                                genus_species = "id to non-insect species") %>%
                                                                                mutate(taxonomy_system = "GBIF")# fill in taxonomy system source
                                                        # if more than one row, select first row
                                                        id_not_insect <- if (nrow(id_not_insect)>1) {id_not_insect[1,]} else {id_not_insect}

                                                        return(id_not_insect)
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
                                 # get genus_species column
                                 if (!("species" %in% colnames(id_acc))) {
                                       id_acc_gs <- id_acc %>% mutate(genus_species = genus)
                                     } else if (is.na(id_acc$species)) {
                                       id_acc_gs <- id_acc %>% mutate(genus_species = genus)
                                     } else {
                                       id_acc_gs <- id_acc %>% mutate(genus_species = species)
                                     }

                                 tax_gbif <- id_acc_gs %>%
                                             # get authority
                                             mutate(taxonomic_authority = ifelse(sapply(strsplit(scientificname, " "), length) == 1,
                                                                                 NA_character_,
                                                                                 gsub("^\\w+\\s+\\w+\\s+(.*)", "\\1", scientificname))) %>%
                                             mutate(taxonomic_authority = ifelse(genus %in% sapply(strsplit(taxonomic_authority, " "), unlist)|
                                                                                 user_supplied_name %in% sapply(strsplit(taxonomic_authority, " "), unlist),
                                                                                 stringr::word(taxonomic_authority,-2,-1),
                                                                                 taxonomic_authority)) %>%
                                             mutate(taxonomy_system = "GBIF") %>% # fill in taxonomy system source
                                             select(-scientificname, -canonicalname, -confidence) %>%
                                             mutate_if(is.logical, as.character)

                                 return(tax_gbif)
                                 }
                             }
                         }

######################


##########################################################################
#' Get taxonomic information from other databases besides GBIF
#'
#' @param taxa_name list of taxa names to search for in other databases
#'
#' @import dplyr
#' @importFrom taxize gnr_resolve
#'
#' @return dataframe
#' @export
#'
#' @examples
get_more_info <- function(taxa_name){
                 id <- taxize::gnr_resolve(sci = taxa_name, data_source_ids=c(1,2,3,4,8,12,152,168,169),
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


#############################################################
#' Get only accepted family-level information from GBIF
#'
#' @param taxa_name genus species name of a taxa
#'
#' @import dplyr
#' @importFrom taxize get_gbifid_
#'
#' @return dataframe
#' @export
#'
#' @examples
get_accepted_families <- function(taxa_name){
                         # get taxa ids, authoritative names, and names higher up
                         id <- taxize::get_gbifid_(taxa_name)  # gets ID from GBIF

                         # deal with cases where species name not found
                         if (nrow(id[[1]]) == 0) {data.frame(user_supplied_name = taxa_name,
                                                             family = "not found")

                         } else {
                         xtra_cols <- c(#"rank", "status", "matchtype", "confidence", "synonym", "acceptedusagekey",
                                        "kingdomkey", "phylumkey", "classkey", "orderkey", "familykey", "note")

                         # puts ID info into one dataframe
                         tax_id <- purrr::map_df(id, ~as.data.frame(.x), .id="user_supplied_name")

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








