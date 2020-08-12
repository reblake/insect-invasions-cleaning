######################################
# updating the taxonomy and occurrence files using Rachael's code

# latest update to tax and occ files on the 12/8/2020
# Purpose to include the intentional_release column for SK and the Causton version of the Galapagos list.

# line 28 loading Rachael's functions
# line 280 set working directory and load raw establishment files and start tax clean
# line 413 to 422 additional check on matches from alternative databases to see if any are actually synonyms in GBIF
# line 600-628 post-cleaning edits (synonyms not recognised by GBIF and Family corrections)
# 636 saving tax file
# 644 starting occ preparation - includes get around for mising "country" and "present_status" columns
# 714 write occ file and final checks
######################################

###################################################

# GBIF Secretariat (2019). GBIF Backbone Taxonomy. Checklist dataset https://doi.org/10.15468/39omei accessed via GBIF.org on 2020-07-10.

####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean taxonomy table            #####
##### created by Rachael Blake    11/15/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(taxize) ; library(rgbif) ; library(purrr)
library(countrycode)



##############################################

# loading Rachael's functions

######################
# a function that cleans the dataframes and separates taxonomy columns
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


######################
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

######################
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
  #  file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
  country_nm <- sapply(strsplit(as.character(df_location), split="_") , function(x) x[1])
  
  
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


# set working directory
setwd("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/raw_by_country")

# checks to see if clean flat files exist, otherwise creates them from multi-worksheet files
# if(!file.exists("./data/raw_data/seebens_clean.csv")|
#    !file.exists("./data/raw_data/raw_by_country/New Zealand_Edney_Browne_2018_clean.xlsx")) {
#            source("./scripts/clean_seebens.R")
#            source("./scripts/clean_new_zealand.R")
#            }

# List all the raw data files
file_list <- dir(pattern='*.xlsx')  # makes list of the files
file_listp <- file_list        # adds path to file names


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
  dplyr::arrange(genus_species) %>% 
  dplyr::filter(!(genus_species == "Baridinae gen"))

# define what taxonomic columns might be named        
tax_class <- c("kingdom", "phylum", "class", "order", "family", "super_family",
               "genus", "species", "genus_species", "taxonomic_authority", "taxonomy_system") 

#####################################
### Make large table with all info

# also correct mis-spellings of certain species based on expert review by A. Liebhold
# misspell <- read_csv("./data/raw_data/taxonomic_reference/misspelling_SAL_resolved.csv", trim_ws = TRUE)

tax_df1 <- tax_df %>% 
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 
  mutate_at(vars(genus_species), str_squish) %>% 
  mutate(user_supplied_name = genus_species) %>% 
  # full_join(misspell, by = "user_supplied_name") %>% 
  # transmute(phylum, class, order, family, super_family, user_supplied_name, 
  #           genus_species = ifelse(!is.na(genus_species.y), genus_species.y, genus_species.x ),
  #           genus = word(genus_species, 1),
  #           species = word(genus_species, 2),
  #           taxonomy_system, taxonomic_authority) %>% 
  distinct(genus_species) %>%  # remove species duplicates          
  dplyr::arrange(genus_species) # arrange alphabetically


#####################################
### Make vectors of genus names (no species info) and species names
# make character vector of names only to genus
# g_sp <- grep('\\<sp\\>', tax_df1$genus_species, value=TRUE) 
# g_spp <- grep('\\<sp.\\>', tax_df1$genus_species, value=TRUE)
g_sp <- filter(tax_df1, (str_count(genus_species, " ") + 1) == 1)
# bard <- grep('\\<gen\\>', tax_df1$genus_species, value=TRUE) # include sub-family here   
tax_vec_gn <- unlist(g_sp, use.names = FALSE) %>%  # gsub(" [a-zA-Z0-9]*", "", .) %>%
  magrittr::extract(!(. == "Tasconotus")) # remove this species



# makes character vector of names only to species 
tax_vec_sp <- tax_df1 %>% 
  filter(!(genus_species %in% g_sp$genus_species))  %>% 
  # magrittr::extract(!(. %in% g_sp)) %>% 
  # magrittr::extract(!(. %in% g_spp)) %>% 
  unlist(., use.names = FALSE) %>% 
  magrittr::extract(!(. == "Baridinae")) # this family put with genus above

#####################################
### Get taxonomy info from GBIF   ###
#####################################
xtra_cols <- c("kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
               "note", "familykey", "genuskey", "scientificname", "canonicalname", "confidence")

######################
# apply the get_accepted_taxonomy function over the vector of species names
tax_acc_l <- lapply(tax_vec_sp, get_accepted_taxonomy) # tax_vec_sp 8440 long


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
tax_go_l <- lapply(go_vec, get_more_info) #395 species

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

# checking if any of these exact match synonyms # found Exochomus quadripustulatus is a synonym of Brumus quadripustulatus
test<-lapply(tax_go$genus_species, get_accepted_taxonomy)
# make dataframe of all results
suppressMessages(
  tax_test <- test %>% 
    purrr::reduce(full_join) %>% 
    mutate(genus_species = str_squish(genus_species)) %>% 
    select(-one_of(xtra_cols))
)

# How many did not return lower rank? 
suppressMessages(
  no_lower <- tax_go_l %>% 
    purrr::reduce(full_join) %>% # join all data frames from list
    # filter to taxa that only returned genus (no new info)
    dplyr::filter((str_count(matched_name2, '\\s+')+1) == 1|
                    matched_name2 == "species not found") 
)

# from no_lower, the not found
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

# make dataframe of matches at species rank
suppressMessages(
  tax_nf <- tax_nf_l %>% 
    purrr::reduce(full_join) %>% 
    dplyr::filter(!(matched_name2 == "species not found")) %>% 
    dplyr::filter((str_count(matched_name2, '\\s+')+1) == 2) %>% 
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

# bring in manual corrections 
sal_taxa <- read_csv("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/taxonomic_references/genus_only_resolution_FIXED.csv", trim_ws = TRUE,
                     col_types = cols(up_to_date_name = col_character()))

# add manual corrections to correct genus-level only matches
genus_match_SAL <- genus_matches %>% 
  left_join(sal_taxa, by = "user_supplied_name") %>% 
  transmute(user_supplied_name, 
            taxonomy_system = ifelse(!(is.na(genus_species.y)), taxonomy_system.y, taxonomy_system.x),
            #kingdom, phylum, class, 
            order, family,
            genus = ifelse(!(is.na(genus_species.y)), word(genus_species.y, 1), genus),
            species = ifelse(!(is.na(genus_species.y)) & rank == "species",
                             genus_species.y, NA_character_),
            genus_species = ifelse(!(is.na(genus_species.y)), genus_species.y, genus_species.x),
            rank, synonym)

manually_matched <- subset(genus_match_SAL, (user_supplied_name %in% sal_taxa$user_supplied_name))

########
# dataframes of remaining unmatched taxa and 
# remaining manual corrections (will be implemented by row replacement below)

# taxa still missing a genus match
still_no_match <- subset(genus_match_SAL, !(user_supplied_name %in% sal_taxa$user_supplied_name))

# taxa included in sal_taxa but not matched in genus_matches (could be from interception data)
man_correct_remain <- subset(sal_taxa, !(user_supplied_name %in% manually_matched$user_supplied_name)) 

########
# put together dataframes with new info

new_sp_info <- tax_nf %>% 
  full_join(tax_go) %>% 
  dplyr::left_join(select(genus_only, user_supplied_name, kingdom,   # this and the transmute adds back in the higher rank info
                          phylum, class, order, family), by = "user_supplied_name") %>% 
  full_join(manually_matched) %>%  # df of manual corrections  
  mutate(genus = ifelse(is.na(genus), word(genus_species, 1), genus),
         rank = ifelse(is.na(rank) & str_count(genus_species, '\\w+')%in% c(2,3),  
                       "species", rank),
         kingdom = ifelse(is.na(kingdom), "Animalia", kingdom),
         phylum = ifelse(is.na(phylum), "Arthropoda", phylum),
         class = ifelse(is.na(class), "Insecta", class))

########
# bring in new non-plant-feeding Australian taxa from Helen
#new_npf_aus <- read_csv("./data/clean_data/new_Aussie_npf_taxa.csv", trim_ws = TRUE, col_types = "cnccccccccccccccn")


#######################################################################
### Combine species list and GBIF accepted names                    ###
#######################################################################
tax_combo <- dplyr::filter(tax_acc, rank %in% c("species", "subspecies")) %>% # GBIF matches to species rank  
  full_join(gen_acc) %>%  # df of taxa where user supplied name was genus only to start with
  # full_join(new_npf_aus) %>%  # df of new non-plant-feeding Australian taxa from Helen
  full_join(new_sp_info, by = "user_supplied_name") %>%  # bind in the new info from auto and manual resolution
  transmute(user_supplied_name,  
            rank = ifelse(is.na(rank.y), rank.x, rank.y),
            status, matchtype, usagekey,
            synonym = ifelse(is.na(synonym.y), synonym.x, synonym.y),
            acceptedusagekey,
            kingdom = ifelse(is.na(kingdom.y), kingdom.x, kingdom.y),
            phylum = ifelse(is.na(phylum.y), phylum.x, phylum.y), 
            class = ifelse(is.na(class.y), class.x, class.y), 
            order = ifelse(is.na(order.y), order.x, order.y), 
            family = ifelse(is.na(family.y), family.x, family.y), 
            genus = ifelse(is.na(genus.y), genus.x, genus.y),
            species = ifelse(is.na(species.y), species.x, species.y),
            genus_species = ifelse(is.na(genus_species.y), genus_species.x, genus_species.y),
            taxonomy_system = ifelse(is.na(taxonomy_system.y), taxonomy_system.x, taxonomy_system.y),
            taxonomic_authority) %>% 
  dplyr::filter(!(is.na(user_supplied_name))) # remove blank rows

# subset remaining manual fixes for those user supplied names that are in tax_combo to get rows that need to be replaced
# rows_2_replace <- subset(man_correct_remain, (user_supplied_name %in% tax_combo$user_supplied_name)) 

# replace rows with new info, and add rows from interception data
tax_final <- tax_combo %>% 
  full_join(man_correct_remain, by = "user_supplied_name") %>% 
  transmute(user_supplied_name, 
            status = ifelse(!is.na(rank.y), NA_character_, status), 
            matchtype = ifelse(!is.na(rank.y), NA_character_, matchtype), 
            usagekey = ifelse(!is.na(rank.y), NA_character_, usagekey), 
            rank = ifelse(!is.na(rank.y), rank.y, rank.x), 
            synonym = ifelse(!is.na(rank.y), synonym.y, synonym.x),
            acceptedusagekey = ifelse(!is.na(rank.y), NA_character_, acceptedusagekey), 
            kingdom, phylum, class, 
            order = ifelse(!is.na(order.y), order.y, order.x),
            family = ifelse(!is.na(family.y), family.y, family.x),
            genus = ifelse(!is.na(family.y), word(genus_species.y, 1), genus),
            species = ifelse(!is.na(family.y), word(genus_species.y, 2), species), 
            genus_species = ifelse(!is.na(genus_species.y), genus_species.y, genus_species.x),
            taxonomy_system = ifelse(!is.na(taxonomy_system.y), taxonomy_system.y, taxonomy_system.x),
            taxonomic_authority = ifelse(!is.na(taxonomic_authority.y), taxonomic_authority.y, taxonomic_authority.x)) %>% 
  mutate(kingdom = ifelse(is.na(kingdom), "Animalia", kingdom),
         phylum = ifelse(is.na(phylum), "Arthropoda", phylum),
         class = ifelse(is.na(class), "Insecta", class),
         genus_species = ifelse(genus_species == "species not found", NA_character_, genus_species)) %>% 
  # add the unique ID column after all unique species are in one dataframe
  tibble::rowid_to_column("taxon_id")

# duplicates
# dups <- tax_final %>% group_by(user_supplied_name) %>% filter(n()>1)

# Bostrichidae
# bos <- tax_combo %>% filter(family == "Bostrichidae")

# Rutelidae, Melolonthidae, and Dynastidae
# RMD <- tax_combo %>% filter(family %in% c("Rutelidae", "Melolonthidae", "Dynastidae"))



# was there any other change I need to make?? tax_final is 9706 rows

##########################
# Family fixes
tax_final[tax_final$family%in%c("Rutelidae","Melolonthidae", "Dynastidae"),"family"]<-"Scarabaeidae"
tax_final[grep("Dermestes",tax_final$genus_species),"family"]<-"Dermestidae"
tax_final[tax_final$genus_species%in%c("Asynonychus godmani","Naupactus godmani"),"genus"]<-"Naupactus"
tax_final[tax_final$genus_species%in%c("Asynonychus godmani","Naupactus godmani"),"genus_species"]<-"Naupactus cervinus"

tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Brachyrhinus sulcatus","genus"]<-"Otiorhynchus"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Brachyrhinus sulcatus","species"]<-"Otiorhynchus sulcatus"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Brachyrhinus sulcatus","genus_species"]<-"Otiorhynchus sulcatus"

tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Platypus rugulosus","genus"]<-"Euplatypus"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Platypus rugulosus","species"]<-"Euplatypus parallelus"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Platypus rugulosus","genus_species"]<-"Euplatypus parallelus"

tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Sitona humeralis","genus"]<-"Sitona"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Sitona humeralis","species"]<-"Sitona discoideus"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Sitona humeralis","genus_species"]<-"Sitona discoideus"

tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Vinsonia stellifera","genus"]<-"Ceroplastes"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Vinsonia stellifera","genus_species"]<-"Ceroplastes stellifer"

tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Diapus borneensis","genus"]<-"Genyocerus"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Diapus borneensis","genus_species"]<-"Genyocerus borneensis"

tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Exochomus quadripustulatus","genus"]<-"Brumus"
tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Exochomus quadripustulatus","genus_species"]<-"Brumus quadripustulatus"


tax_final[!is.na(tax_final$genus_species)&tax_final$genus_species=="Trichapion pauper","rank"]<-"species"


#####################################
### Write file                    ###
#####################################
# write the clean taxonomy table to a CSV file
readr::write_csv(tax_final, "taxonomy_table.csv")
###############################################

#####################################
### Making the occurrence table   ###
#####################################

# apply that function over the list of dataframes
occurr_list <- lapply(file_listp, separate_occurrence) 
occurr_list[[3]]$country<-"Galapagos"
occurr_list[[3]]$present_status<-"NA"
# put all occurrence dataframes into one large dataframe
df_occurr <- occurr_list %>% 
  purrr::reduce(full_join) %>% 
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
  # remove Arachnid
  filter(!(genus_species == "Trixacarus caviae")) %>% 
  # fill in country column with canada_or_us info
  mutate(country = ifelse(is.na(country) & canada_or_us %in% c("Canada", "Us", "Us, may not actually be adventive"), 
                          canada_or_us, country),
         present_status = ifelse(present_status == "Na", NA, present_status),
         notes = ifelse(country == "Us, may not actually be adventive", "may not actually be adventive", ""),
         country = ifelse(country == "Us, may not actually be adventive", "Us", country),
         notes = ifelse(origin == "New insect record for 1960  purposeful introduction", 
                        "New insect record for 1960  purposeful introduction", ""),
         origin = ifelse(origin == "New insect record for 1960  purposeful introduction",
                         "", origin),
         notes = ifelse(origin == "New insect record for 1963, chance immigrant", 
                        "New insect record for 1963, chance immigrant", ""),
         origin = ifelse(origin == "New insect record for 1963, chance immigrant",
                         "", origin)
  ) %>% 
  # clean up/fill in country column
  mutate(year = ifelse(year == -999, NA, year),
         country = ifelse(region %in% c("Okinawa", "Ogasawara", "Japan"), "Japan", country),
         country = ifelse(region == "Hawaii", "Us", country),
         country = ifelse(region == "Korea", "Korea", country),
         country = ifelse(region == "New Zealand", "New Zealand", country),
         notes = ifelse(grepl("Proceedings of the", .$origin), origin, notes),
         origin = ifelse(grepl("Proceedings of the", .$origin), "", origin)) %>% 
  # clean up some species names
  mutate(genus_species = gsub("Mycetophila\xa0propria", "Mycetophila propria", genus_species),
         genus_species = gsub("Mycetophila\xa0vulgaris", "Mycetophila vulgaris", genus_species),
         genus_species = gsub("Mycetophila\xa0marginepunctata", "Mycetophila marginepunctata", genus_species),
  ) %>%         
  # clean up year column
  mutate(year = ifelse(year == "N/A", NA_character_, year),
         year = gsub("\\s", "", year, perl=TRUE)) %>% 
  # clean up intentional release column
  mutate(intentional_release = ifelse(intentional_release %in% c("N"), "No", 
                                      ifelse(intentional_release %in% c("1", "I"), "Yes", intentional_release))) %>% 
  # add country codes for country and origin columns
  # mutate(country_code = countrycode(country, "country.name", "iso3n", warn = TRUE),
  #       origin_code = countrycode(origin, "country.name", "iso3n", warn = TRUE)) %>% 
  mutate(genus_species = gsub("\xa0", " ", genus_species , perl=TRUE)) %>% # trying to get rid of weird characters
  dplyr::select(-canada_or_us, -nz_region) %>% 
  dplyr::arrange(genus_species) 

# add the unique ID column and delete genus species column(s)
tax_table <- read.csv("taxonomy_table.csv", stringsAsFactors=F)  # read in the taxonomy table

# make final occurrence dataframe
occurr_df <- df_occurr %>%
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>%  # remove rogue white spaces
  dplyr::rename(user_supplied_name = genus_species) %>% # have to rename genus_species to user_supplied_name so matches are correct
  dplyr::left_join(y = select(tax_table, c(user_supplied_name, taxon_id, genus_species)),
                   by = "user_supplied_name") %>% # join in the taxonomy info
  mutate(genus_species = gsub("<a0>", " ", genus_species, perl=TRUE)) %>% 
  select(taxon_id, everything()) %>% # make taxon_id column the first column
  dplyr::arrange(region) # order by region


#####################################
### Write file                    ###
#####################################
# write the clean occurrence table to a CSV file
readr::write_csv(occurr_df, "occurrence_table.csv") #12667

# comparing occurance and taxonomy table to old files
# new files are occurr.df and tax_final

tax_old<-read.csv("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/raw_by_country/taxonomy_table2020Jul10.csv",stringsAsFactors = FALSE)
setdiff(tax_final$genus_species,tax_old$genus_species)
setdiff(tax_old$genus_species,tax_final$genus_species)
#############################################
# Final checks on old tax occ table vs new tax and occur table

tax_new<-read.csv("taxonomy_table.csv",stringsAsFactors = FALSE)
tax_old<-read.csv("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/raw_by_country/taxonomy_table2020Jul10.csv",stringsAsFactors = FALSE)

occ_new<-read.csv("occurrence_table.csv",stringsAsFactors = FALSE)
occ_old<-read.csv("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/raw_by_country/occurrence_table2020Jul10.csv",stringsAsFactors = FALSE)

temp<-setdiff(tax_new$genus_species,tax_old$genus_species)

check<-occ_new[occ_new$genus_species%in%temp,]

temp<-setdiff(tax_old$genus_species,tax_new$genus_species)
check<-occ_old[occ_old$genus_species%in%temp,]

# they seem to be alright "Alloxysta fuscicornis" "Limonia advena" are not recognised in the new files though even though they are there.

# what percentage were resolved in gbif?

nrow(tax_new[tax_new$taxonomy_system=="GBIF",])/nrow(tax_new)
nrow(tax_new[tax_new$taxonomy_system=="GBIF"&tax_new$rank=="species"&tax_new$order=="Coleoptera",])/nrow(tax_new[tax_new$rank=="species"&tax_new$order=="Coleoptera",])
