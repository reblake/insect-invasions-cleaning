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
res <- name_lookup(higherTaxonKey = key, limit = 2000, status = c("accepted", "doubtful"))
ins_orders <- res$data %>% filter(rank == "ORDER") %>% select(canonicalName, key, rank) %>% 
              filter(!canonicalName %in% c("Coleoptera", "Hymenoptera", "Lepidoptera", "Diptera"))
ins_ord_keys <- unlist(ins_orders$key, use.names = FALSE) # unlist the keys so they can be used by lapply

# get family keys for orders with lots of species
ins_fams <- res$data %>% 
            filter(order %in% c("Coleoptera", "Hymenoptera", 
                                        "Lepidoptera", "Diptera"),
                   rank == "FAMILY") %>% 
            select(canonicalName, order, key, rank) %>% 
            filter(!key == 3519, # THERE'S A PROBLEM WITH 3519 Tethinidae and 4753 Propalticidae: IT HANGS FOREVER AND DOESN'T 
                   !key == 4753, 
                   !key == 6925,
                   !key == 7298,
                   !key == 7827,
                   !key == 8662,
                   !key == 8842,
                   !key == 9406,
                   !key == 9485) # PULL IN DATA  
ins_fam_keys <- unlist(ins_fams$key, use.names = FALSE)
            
problem_keys <- c(3519, 4753, 6925, 7298, 7827, 8662, 8842, 9406, 9485)  
problem_df <- res$data %>% filter(key %in% problem_keys) %>% select(canonicalName, order, key, rank)
# Spaniidae	Diptera	6925, Conopidae	Diptera	7298, 
# Endecatomidae	Coleoptera	7827, Acrolepiidae	Lepidoptera	8662,	Crinopterygidae	Lepidoptera	8842,
# Mirinidae	Lepidoptera	9406,	Eurychoromyiidae	Diptera	9485

# define function to get phylo trees with counts
get_phylo_counts <- function(key){
                    Sys.sleep(5) 
                    # get lower taxonomic
                    res <- name_lookup(higherTaxonKey = key, limit = 99000, status = c("accepted", "doubtful")) 
                    i_df <- res$data %>% select(-datasetKey, -constituentKey, -scientificName, -parentKey) 
                    i_df$threatStatuses <- as.character(i_df$threatStatuses)
                    
                    if(nrow(i_df > 0)){print(paste("success", key))}else{print("failure")}
                    
                    return(i_df) 
                    }

# orders
order_list <- lapply(ins_ord_keys, get_phylo_counts) 

df_orders <- order_list %>% 
             purrr::reduce(full_join)

# families
# t_list <- c(4542, 4546, 4547, 4548, 4549, 4554, 4731, 4732, 4733, 4734, 4735, 4736, 4737, 4738, 4739, 4740, 4741, 4742,
#             4743, 4744, 4745, 4746, 4747, 4748, 4749, 4750, 4751, 4752, 4753)
# t_list <- ins_fam_keys[477:517]
# t_fami_list <- lapply(t_list, get_phylo_counts)

fami_list <- lapply(ins_fam_keys, get_phylo_counts)

df_fams <- map_dfr(fami_list, ~{.x %>%    # brilliant piece of code from here: https://community.rstudio.com/t/simplest-way-to-modify-the-same-column-in-multiple-dataframes-in-a-list/13076
                   mutate(habitats = as.character(habitats))
                   })
  

# join orders and families
df_combo <- bind_rows(df_orders, df_fams)

# write out dataframe
readr::write_csv(df_combo, "/nfs/insectinvasions-data/data/clean_data/phylo_counts.csv")

