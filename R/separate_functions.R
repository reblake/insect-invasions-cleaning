####################################################################
##### Functions for separating taxonomic and related info      #####
##### originally created by Rachael Blake    04/11/2019        #####
####################################################################

######################
#' separate_taxonomy_xl: a function that cleans the dataframes and separates taxonomy columns using xl files
#'
#' @param df_location location of files containing raw insect data
#'
#' @import dplyr
#' @importFrom readxl read_excel
#' @importFrom tidyselect one_of
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' file_list <- system.file("extdata", "Japan_taxa.xlsx", package = "insectcleanr", mustWork = TRUE)
#' taxa_list <- lapply(file_list, separate_taxonomy_xl)
#' }
separate_taxonomy_xl <- function(df_location){
                        # reads the excel file in
                        df <- readxl::read_excel(df_location, trim_ws = TRUE, col_types = "text")

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
                                select(tidyselect::one_of(tax_class)) %>%
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
                                       genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE),
                                       genus_species = gsub("[A-Z]\\b", "", genus_species, perl=TRUE)
                                       )

                        # return df_2
                        return(df_2)
                        }

######################
#' separate_taxonomy_csv: a function that cleans the dataframes and separates taxonomy columns using csv files
#'
#' @param df_location location of files containing raw insect data
#'
#' @import dplyr
#' @importFrom tidyselect one_of
#' @importFrom utils read.csv
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' file_list <- system.file("extdata", "Japan_taxa.xlsx", package = "insectcleanr", mustWork = TRUE)
#' taxa_list <- lapply(file_list, separate_taxonomy_csv)
#' }
separate_taxonomy_csv <- function(df_location){
                         # reads the excel file in
                         df <- read.csv(df_location, strip.white = TRUE)

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
                                 select(tidyselect::one_of(tax_class)) %>%
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
                                        genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE),
                                        genus_species = gsub("[A-Z]\\b", "", genus_species, perl=TRUE)
                                        )

                         # return df_2
                         return(df_2)
                         }


###########################################################
#' separate_occurrence_xl: function to read in and separate occurrence info using xl files
#'
#' @param df_location location of files containing taxa info
#'
#' @import dplyr
#' @importFrom readxl read_excel
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' my_df <- "/path/to/my_df.csv"
#' attrib <- separate_occurrence_xl(my_df)
#' }
separate_occurrence_xl <- function(df_location){
                          # reads the excel file in
                          df <- readxl::read_excel(df_location, trim_ws = TRUE, col_types = "text")

                          # clean up column names, capitalization, etc.
                          df_1 <- df %>%
                                  # replace " " and "." with "_" in column names
                                  select_all(~gsub("\\s+|\\.", "_", .)) %>%
                                  select_all(tolower) %>%  # make all column names lower case
                                  mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                                   "\\U\\1\\L\\2", . , perl=TRUE))

                          # define region
                          file_name <-  str_extract(df_location, "[^\\/]+$")
                          country_nm <-  str_extract(file_name, "^[^\\_]+")


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


###########################################################
#' separate_occurrence_csv: function to read in and separate occurrence info using csv files
#'
#' @param df_location location of files containing taxa info
#'
#' @import dplyr
#' @importFrom utils read.csv
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' my_df <- "/path/to/my_df.csv"
#' attrib <- separate_occurrence_csv(my_df)
#' }
separate_occurrence_csv <- function(df_location){
                          # reads the csv file in
                          df <- read.csv(df_location, strip.white = TRUE)

                          # clean up column names, capitalization, etc.
                          df_1 <- df %>%
                                  # replace " " and "." with "_" in column names
                                  select_all(~gsub("\\s+|\\.", "_", .)) %>%
                                  select_all(tolower) %>%  # make all column names lower case
                                  mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                                   "\\U\\1\\L\\2", . , perl=TRUE))

                          # define region
                          file_name <-  str_extract(df_location, "[^\\/]+$")
                          country_nm <-  str_extract(file_name, "^[^\\_]+")


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



############################################################
#' separate_attributes_xl: function to separate attribute info for each taxa from the raw files using xl files
#'
#' @param df_location location of file containing taxa info
#'
#' @import dplyr
#' @importFrom readxl read_excel
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' my_df <- "/path/to/my_df.csv"
#' attrib <- separate_attributes_xl(my_df)
#' }
separate_attributes_xl <- function(df_location){
                          # reads the excel file in
                          df <- readxl::read_excel(df_location, trim_ws = TRUE, col_types = "text")

                          # clean up column names, capitalization, etc.
                          df_1 <- df %>%
                                  # replace " " and "." with "_" in column names
                                  select_all(~gsub("\\s+|\\.", "_", .)) %>%
                                  select_all(tolower) %>%  # make all column names lower case
                                  mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                                   "\\U\\1\\L\\2", . , perl=TRUE))

                          # define region
                          file_name <-  str_extract(df_location, "[^\\/]+$")
                          country_nm <-  str_extract(file_name, "^[^\\_]+")

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



############################################################
#' separate_attributes_csv: function to separate attribute info for each taxa from the raw files using csv files
#'
#' @param df_location location of file containing taxa info
#'
#' @import dplyr
#' @importFrom utils read.csv
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' my_df <- "/path/to/my_df.csv"
#' attrib <- separate_attributes_csv(my_df)
#' }
separate_attributes_csv <- function(df_location){
                           # reads the excel file in
                           df <- read.csv(df_location, strip.white = TRUE)
                           # clean up column names, capitalization, etc.
                           df_1 <- df %>%
                                   # replace " " and "." with "_" in column names
                                   select_all(~gsub("\\s+|\\.", "_", .)) %>%
                                   select_all(tolower) %>%  # make all column names lower case
                                   mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                                    "\\U\\1\\L\\2", . , perl=TRUE))

                           # define region
                           file_name <-  str_extract(df_location, "[^\\/]+$")
                           country_nm <-  str_extract(file_name, "^[^\\_]+")

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



###########################################################
#' separate_source_csv: function to read in and separate source info using csv files
#'
#' @param df_location location of files containing taxa info
#'
#' @import dplyr
#'
#' @return dataframe
#' @export
#'
#' @examples
#' \dontrun{
#' my_df <- "/path/to/my_df.csv"
#' }

separate_source_csv <- function(df_location){
                       # reads the csv file in
                       df <- read.csv(df_location, strip.white = TRUE)

                       # clean up column names, capitalization, etc.
                       df_1 <- df %>%
                               # replace " " and "." with "_" in column names
                               select_all(~gsub("\\s+|\\.", "_", .)) %>%
                               select_all(tolower) %>%  # make all column names lower case
                               mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                                                "\\U\\1\\L\\2", . , perl=TRUE))

                       # define region
                       file_name <-  str_extract(df_location, "[^\\/]+$")
                       country_nm <-  str_extract(file_name, "^[^\\_]+")

                       # define what relevant columns to select - note that the original column
                       # called genus_species will become the user_supplied_name column
                       source_columns <- c("genus_species", "year","intentional_release","eradicated",
                                           "established_indoors_or_outdoors","source")

                       # split off any columns with any taxonomic column names
                       df_2 <- df_1 %>%
                               select(tidyselect::one_of(source_columns)) %>%
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

