#############################################
## Core section of taxonomic cleaning code ##
## 28 March 2019 added by Rebecca ###########
#############################################

## tax_list is a dataframe with a column of species names to clean.
## Prefereably these names are lower case with only the first letter in upper case
## Names which are only to genus level can end in sp
## remove nr., cf. 

names(tax_list)<-"genus_species"

#######################################
##### Run modified cleaning code ######
#######################################

# put all taxonomy dataframes into one large dataframe
tax_df <- tax_list %>% 
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>%
  #  dplyr::rename(taxonomic_authority = authority)} %>% # Rebecca Removed this line as interception data has no taxonomic authority column
  dplyr::arrange(genus_species) 

# define what taxonomic columns might be named        
tax_class <- c("kingdom", "phylum", "class", "order", "family", "super_family",
               "genus", "species", "genus_species", "taxonomic_authority", "taxonomy_system") 




#####################################
### Make large table with all info

tax_df1 <- tax_df %>% 
  #bind_rows(seeb_data1) %>% 
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 
  mutate_at(.vars = vars(genus_species), .funs = funs(str_squish)) %>% 
  distinct(genus_species) %>%  # remove species duplicates          
  dplyr::arrange(genus_species) # arrange alphabetically


#####################################
### Get taxonomy info from GBIF   ###
#####################################
# makes character vector of species names
tax_vec <- unlist(tax_df1$genus_species, use.names = FALSE) #20


# write function(s) to apply over this character vector


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
        dplyr::filter(if (sum(status %in% c("ACCEPTED") & matchtype %in% c("EXACT"))>0){ 
          status == "ACCEPTED" & matchtype == "EXACT"
        } else if (sum(status %in% c("SYNONYM") & matchtype %in% c("EXACT")& rank!="genus")>0) {
          status == "SYNONYM" & matchtype == "EXACT" & rank!="genus"
        } else if (sum(status %in% c("DOUBTFUL") & matchtype %in% c("EXACT"))>0) {
          status == "DOUBTFUL" & matchtype == "EXACT"
        } else if (sum(status %in% c("ACCEPTED") & matchtype %in% c("HIGHERRANK"))>0) {
          status == "ACCEPTED" & matchtype == "HIGHERRANK"
        } else if (sum(status %in% c("DOUBTFUL") & matchtype %in% c("HIGHERRANK"))>0) {
          status == "DOUBTFUL" & matchtype == "HIGHERRANK"
        } else if (sum(status %in% c("SYNONYM") & matchtype %in% c("EXACT")& rank=="genus")>0) {
          status == "SYNONYM" & matchtype == "EXACT" & rank=="genus"
        } else if (sum(status %in% c("ACCEPTED") & matchtype %in% c("FUZZY"))>0) {
          status == "ACCEPTED" & matchtype == "FUZZY"
        } else if (sum(status %in% c("SYNONYM") & matchtype %in% c("FUZZY"))>0) {
          status == "SYNONYM" & matchtype == "FUZZY" 
        } else {row_number() == 1 
        }) %>%  
        dplyr::filter(xor(any(rank %in% c("species", "subspecies", "form")), 
                          any(rank %in% c("genus","family","order","class","phylum","kingdom")))) %>% # filter rank to species if both genus and species, and filter to genus if both genus and family # Rebecca changed, also the mutate lines below
        select(-one_of(xtra_cols)) 
      
      id_acc <- if (nrow(id_acc)>1) {id_acc[1,]} else {id_acc} # if more than one row, select first row
      
      # make df of all taxonomic info from GBIF
      tax_gbif <- id_acc %>%
        # get authority
        mutate(taxonomic_authority = ifelse(sapply(strsplit(scientificname, " "), length) == 1,
                                            NA_character_,
                                            gsub("^\\w+\\s+\\w+\\s+(.*)", "\\1", scientificname))) %>%
        mutate(taxonomic_authority = ifelse((exists("genus")&&(genus %in% sapply(strsplit(taxonomic_authority, " "), unlist)))|
                                              user_supplied_name %in% sapply(strsplit(taxonomic_authority, " "), unlist),
                                            stringr::word(taxonomic_authority,-2,-1),
                                            taxonomic_authority)) %>% 
        # get genus_species
        mutate(genus_species = ifelse((!exists("species")|is.na("species"))&exists("genus"), 
                                      paste(genus, "sp"), ifelse(exists("species"),species,"undetermined")))  %>%
        mutate(taxonomy_system = "GBIF") %>% # fill in taxonomy system source
        select(-scientificname, -canonicalname, -confidence) %>% 
        mutate_if(is.logical, as.character) 
      
      return(tax_gbif)
    }
  }
}


# apply the function over the vector of species names
tax_acc_l <- lapply(tax_vec, get_accepted_taxonomy) 

xtra_cols <- c("kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
               "note", "familykey", "genuskey", "scientificname", "canonicalname", "confidence")

# make dataframe of all results
suppressMessages(
  tax_acc <- tax_acc_l %>% 
    purrr::reduce(full_join) %>% 
    mutate(genus_species = str_squish(genus_species)) %>% 
    select(-one_of(xtra_cols))
)
