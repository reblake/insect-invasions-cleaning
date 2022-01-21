#######################################################
# separate_source draft start started 21 Jan 2022 by RT

# framework copied from the separate_occurence_csv function in RB's separate_functions.R script in the new-functions branch

#######################################################

###########################################################
#' separate_occurrence_csv: function to read in and separate occurrence info using csv files
#'
#' @param df_location location of files containing taxa info
#'
#' @import dplyr
#'
#' @return dataframe
#' @export
#'
#' @examples
separate_source_csv <- function(df_location){ # RT renamed function
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
  
  # define region
  file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
  country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
  
  # define what relevant columns to select - note that the original column caled genus_species will become the user_supplied_name column
  source_columns <- c("genus_species", "year","intentional_release","eradicated","established_indoors_or_outdoors","source")
  
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

