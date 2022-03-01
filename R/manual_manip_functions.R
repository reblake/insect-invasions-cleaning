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
#' \dontrun{
#' file <- system.file("extdata", "Japan_taxa.xlsx", package = "insectcleanr", mustWork = TRUE)
#' data_j <- read_excel(file)
#' data_co <- coalesce_manual(data_j)
#' }
coalesce_manual <- function(df) {
                   # test whether there are multiple rows
                   if(nrow(df) == 1){coal_manual <- df %>%
                                                    mutate_at(vars(genus_species, plant_feeding,
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
                                  select(genus_species, plant_feeding, intentional_release, ever_introduced_anywhere,
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
                                   mutate_at(vars(genus_species, plant_feeding,
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



#######################################################
#' Title: function to manually coalesce rows in occurrence table
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
coalesce_occur <- function(df) {
                  # test whether there are multiple rows
                  if(nrow(df) == 1) {no_dup <- df %>%
                                               select(genus_species, year, region, country, origin,
                                                      host_type, ecozone, intentional_release,
                                                      established_indoors_or_outdoors, confirmed_establishment, eradicated,
                                                      present_status) %>%
                                               mutate_at(vars(genus_species, region, country,
                                                              host_type, origin, ecozone, intentional_release,
                                                              established_indoors_or_outdoors, confirmed_establishment,
                                                              eradicated, present_status),
                                                         list(as.character)) %>%
                                               mutate_at(vars(year), list(as.numeric))
                     } else {

                       # coalesce to earliest year
                       yr <- df %>%
                             select(genus_species, region, year) %>%
                             group_by(genus_species, region) %>%
                             # summarize_all( ~ ifelse(nrow(year)>1 , , year)) %>%
                             filter(rank(year, ties.method = "first") == 1) %>%
                             ungroup() %>%
                             mutate_at(vars(year), list(as.numeric))

                       # take out duplicates in the genus_species/region columns
                       gsr_dp <- df %>%
                                 select(-year) %>%
                                 group_by(genus_species, region) %>%
                                 summarize_all(DescTools::Mode, na.rm = TRUE) %>%
                                 ungroup() %>%
                                 select(genus_species, region, country, origin,
                                        host_type, ecozone, intentional_release,
                                        established_indoors_or_outdoors, confirmed_establishment, eradicated,
                                        present_status) %>%
                                 mutate_at(vars(genus_species, region, country,
                                                host_type, origin, ecozone, intentional_release,
                                                established_indoors_or_outdoors, confirmed_establishment,
                                                eradicated, present_status),
                                           list(as.character))

                       # put year and everything else back together
                       no_dup <- gsr_dp %>%
                                 full_join(yr) %>%
                                 select(genus_species, year, region, country, origin,
                                        host_type, ecozone, intentional_release,
                                        established_indoors_or_outdoors, confirmed_establishment, eradicated,
                                        present_status)

                     }
                  return(no_dup)
                  }

