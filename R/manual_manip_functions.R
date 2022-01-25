####################################################################
##### Functions for manipulating taxonomic and related info    #####
##### originally created by Rachael Blake    04/11/2019        #####
####################################################################


#######################################################
#' coalesce_manual: function to manually coalesce rows in attribute table
#'
#' @param df dataframe that may need coalescing
#'
#' @import dplyr
#' @importFrom DescTools Mode
#'
#' @return dataframe
#' @export
#'
#' @examples
#' file <- system.file("extdata", "Japan_taxa.xlsx", package = "insectcleanr", mustWork = TRUE)
#' data_j <- read_excel(file)
#' data_co <- coalesce_manual(data_j)
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



