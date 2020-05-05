########################################################
##### Script to download counts of downstream taxa #####
##### for Coleoptera   5/1/2020                    #####
########################################################

library(tidyverse) ; library(rgbif)

### This info comes from the GBIH backbone taxonomy.

get_order_fams <- function(order_name){
                  # get key for Order 
                  key <- name_backbone(order_name)$usageKey       
                  # get lower taxonomic
                  res <- name_lookup(higherTaxonKey = key, limit = 7000, rank = "family",
                                     status = c("accepted", "doubtful"))  
                  # get families in Order
                  order_fams <- res$data %>% 
                                filter(rank == "FAMILY") %>% 
                                select(canonicalName, order, key, rank)
                  order_fam_keys <- unlist(order_fams$key, use.names = FALSE)
                  
                  return(order_fam_keys)
                  }

# apply this function to get list of family keys in the Order
families <- get_order_fams("Coleoptera")
# curl fetch was timing out, so broke this familiest list up 
families1 <- families[1:157]   
# Code hangs on these families, likely because no species children listed in GBIF, so removing.
# Key 4491 = family Eccoptarthridae 
# Key 4753 = family Propalticidae
# Key 4759 = family Rhysodidae
# Key 6983 = family Georyssidae
# Key 7827 = family Endecatomidae
families1 <- families1[!families1 %in% c(4491, 4753, 4759, 6983, 7827)]
families2 <- families[158:364]
# Code hangs on these families, likely because no species children listed in GBIF, so removing.
# Key 3260381 = family Ceratocanthidae
# Key 3260406 = family Cephaloidae
# Key 3260418 = family Cebrionidae
# Key 3261178 = family Artematopidae
# Key 3262455 = family Sphaerosomatidae
# Key 3262720 = family Rhizophagidae
# Key 3262857 = family Platypodidae
# Key 3262909 = family Perimylopidae
# Key 3263122 = family Monommidae
# Key 3263295 = family Leptinidae
# Key 3263306 = family Lepiceridae
# Key 3263463 = family Homalisidae  
# Key 3263768 = family Discolomidae
# Key 3263801 = family Dasyceridae
# Key 4290524 = family Cneoglossidae
# Key 4290614 = family Hobartiidae
# Key 4290627 = family Agapythidae
# Key 4290628 = family Tasmosalpingidae 
# Key 4290643 = family Pterogeniidae
# Key 4290659 = family Trachelostenidae
# Key 4290672 = family Metaxinidae
# Key 4290674 = family Phycosecidae
# Key 4290676 = family Attalomimidae
# Key 4293564 = family Microsporidae
# Key 4426147 = family Lyctidae
# Key 4747114 = family Micropeplidae
# Key 4747799 = family Peltidae
# Key 4762144 = family Taldycupedidae
# Key 4802225 = family Tricoleidae
# Key 4946274 = family Empelidae
# Key 4946282 = family Acanthoceridae
# Key 4946292 = family Allocorynidae
# Key 6066946 = family Podabrocephalidae
# Key 6141834 = family Merophysiidae
# Key 6141835 = family Cryptophilidae
# Key 6757792 = family Vesperidae
# Key 6880232 = family Cyclaxyridae
# Key 6880401 = family Tritarsidae
# Key 6880493 = family Triadocupedidae
# Key 6880538 = family Sinisilvanidae
# Key 6880559 = family Colymbothetidae
# Key 6969559 = family Meruidae
# Key 6992941 = family Pallichnidae
# Key 6993905 = family Oxypeltidae
# Key 6994219 = family Akalyptoischiidae
# Key 7000927 = family Ischaliidae
# Key 7466140 = family Cimberididae
# Key 8445006 = family Bruchelae
# Key 8446328 = family Necrophagi
# Key 8456795 = family Mordellonae
# Key 8471713 = family Cucujipes
# Key 8473864 = family Clerii
# Key 8475195 = family Scarabaeides
# Key 8483091 = family Diaperialae
# Key 8486178 = family Tridigitati
# Key 8491332 = family Scolitarii
# Key 8508379 = family Paelobiidae
# Key 8510594 = family Sternoxi
# Key 8518820 = family Xylophagi
# Key 8538401 = family Cicindeletae
# Key 8547462 = family Ptiniores
# Key 8549759 = family Sphaeridiota
# Key 8551168 = family Erotilenae
# Key 8579869 = family Chrysomelinae
# Key 8589822 = family Malacodermidae
# Key 8597409 = family Staphyliniae
# Key 8600253 = family Cebrionates
# Key 8601248 = family Curculionites
# Key 8618253 = family Helopii
# Key 8631634 = family Bostrichini
# Key 8633045 = family Byrrhii
# Key 8648423 = family Cerambicini
# Key 8649925 = family Nitidulariae
# Key 8652426 = family Coprophagi
# Key 8662155 = family Pselaphii
# Key 8679815 = family Trogossitarii
# Key 8682319 = family Malacodermi
# Key 8683905 = family Macrogastri
# Key 8693120 = family Geotrupini
# Key 8696748 = family Tenebrionites
# Key 9272313 = family Dytiscides
# Key 9272654 = family Hydrocanthari
# Key 9366619 = family Erithynidae
# Key 9392869 = family Haplochelidae
# Key 9397256 = family Passalida
# Key 9412508 = family Anthribides
# Key 9432487 = family Scraptiaeidae
# Key 9452449 = family Salpingides
# Key 9475651 = family Dryopides
# Key 9515631 = family Monotomites
# Key 9534620 = family Helophorida
# Key 9544026 = family Leiodesidae
# Key 9562083 = family Lymoxylonidae
# Key 9577114 = family Mycetophagides
# Key 9618539 = family Cerylonides
# Key 10354733 = family Thymalidae
# Key 10489490 = family Rhadalidae
families2 <- families2[!families2 %in% c(3260381, 3260406, 3260418, 3261178, 3262455, 
                                         3262720, 3262857, 3262909, 3263122, 3263295, 
                                         3263306, 3263463, 3263768, 3263801, 4290524, 
                                         4290614, 4290627, 4290628, 4290643, 4290659, 
                                         4290672, 4290674, 4290676, 4293564, 4426147,
                                         4747114, 4747799, 4762144, 4802225, 4946274,
                                         4946282, 4946292, 6066946, 6141834, 6141835,
                                         6757792, 6880232, 6880401, 6880493, 6880538,
                                         6880559, 6969559, 6992941, 6993905, 6994219,
                                         7000927, 7466140, 8445006, 8446328, 8456795,
                                         8471713, 8473864, 8475195, 8483091, 8486178,
                                         8491332, 8508379, 8510594, 8518820, 8538401,
                                         8547462, 8549759, 8551168, 8579869, 8589822,
                                         8597409, 8600253, 8601248, 8618253, 8631634,  
                                         8633045, 8648423, 8649925, 8652426, 8662155,
                                         8679815, 8682319, 8683905, 8693120, 8696748,  
                                         9272313, 9272654, 9366619, 9392869, 9397256,
                                         9412508, 9432487, 9452449, 9475651, 9515631, 
                                         9534620, 9544026, 9562083, 9577114, 9618539,  
                                         10354733, 10489490)]

## Function to get phylo counts; copied from `phylo_tree_info.R`
# define function to get phylo trees with counts
get_phylo_counts <- function(key){
                    Sys.sleep(3) 
                    # get lower taxonomic
                    res <- name_lookup(higherTaxonKey = key, limit = 99000, rank = "species",
                                       status = c("accepted", "doubtful")) 
                    i_df <- res$data %>% select(-datasetKey, -constituentKey, -scientificName, -parentKey) 
                    i_df$threatStatuses <- as.character(i_df$threatStatuses)
  
                    if(nrow(i_df > 0)){print(paste("success", key))}else{print("failure")}
  
                    return(i_df) 
                    }

fams_list1 <- lapply(families1, get_phylo_counts) 
fams_list2 <- lapply(families2, get_phylo_counts)
fams_list <- c(fams_list1, fams_list2)

# function to pare down the returned information to just number of species per family
compute_only_counts <- function(family_list){
                       df_fams <- map_dfr(family_list, ~{.x %>%    # brilliant piece of code from here: https://community.rstudio.com/t/simplest-way-to-modify-the-same-column-in-multiple-dataframes-in-a-list/13076
                                          mutate(habitats = as.character(habitats))
                                          })
                       sp_counts <- df_fams %>% 
                                    filter(rank == "SPECIES") %>% 
                                    group_by(family) %>%
                                    mutate(sp_per_fam = n()) %>% 
                                    ungroup() %>% 
                                    select(kingdom, phylum, order, family, familyKey, sp_per_fam) %>% 
                                    distinct()
                       
                       return(sp_counts)     
                       }

df_sp_counts <- compute_only_counts(fams_list)

# write out dataframe
readr::write_csv(df_sp_counts, "/nfs/insectinvasions-data/data/clean_data/coleoptera_counts.csv")



