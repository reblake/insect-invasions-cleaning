########################################################
##### Script to download counts of downstream taxa #####
##### 8/21/2019                                    #####
########################################################

# Resource for code: 
# https://discuss.ropensci.org/t/some-thoughts-on-working-with-rgbif-occurrence-data-including-mapping/1105

library(tidyverse) ; library(rgbif)

# get list of all orders in Class Insecta
key <- name_backbone("Insecta")$usageKey  # get the key
# get lower taxonomic
res <- name_lookup(higherTaxonKey=key, limit = 50, status = c("accepted", "doubtful"))
ins_orders <- res$data %>% filter(rank == "ORDER") %>% select(canonicalName, key, rank)

ins_keys <- unlist(ins_orders$key, use.names = FALSE) # unlist the keys so they can be used by lapply

# define function to get phylo trees with counts
get_phylo_counts <- function(key){
                    Sys.sleep(3) 
                    # get lower taxonomic
                    res <- name_lookup(higherTaxonKey = key, limit = 99000, status = c("accepted", "doubtful")) 
                    i_df <- res$data %>% select(-datasetKey, -constituentKey, -scientificName, -nubKey, -parentKey,
                                                -basionymKey, -basionym) 
                    
                    return(i_df) 
                    }

in_k <- c(1451, 584, 1457)
order_list <- lapply(in_k, get_phylo_counts) 

df_orders <- order_list %>% 
             purrr::reduce(full_join)


