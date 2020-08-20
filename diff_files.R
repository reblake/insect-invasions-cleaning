#################
###  Diff between Rebecca Turner's taxonomy file and the authoritative taxonomy table
################

library(diffobj) ; library(tidyverse) ; library(compareDF)

# Rebecca's file
rt_aug_file <- read.csv("nfs_data/data/clean_data/taxonomy_table2020Aug12b.csv")
rt_aug_file <- rt_aug_file %>% arrange(taxon_id)

# taxonomy table
tax_table <- read.csv("nfs_data/data/clean_data/taxonomy_table.csv")
tax_table <- tax_table %>% arrange(taxon_id)

nrow(rt_aug_file) ; nrow(tax_table)  # 2 rows difference
names(rt_aug_file) ; names(tax_table)

################################################
# Diff between the two files:

# diffPrint(target = rt_aug_file, current = tax_table)
# 
# # only returns two rows of differences, but unintelligible result

################################################

# Use another method to Diff the two files: 

diff1 <- setdiff(tax_table, rt_aug_file)
str(diff1)

################################################

# # Use another method to Diff the two files: 
# 
# dplyr::all_equal(tax_table, rt_aug_file)  # not useful!  already get this from nrow() above!


################################################

# # Use another method to Diff the two files: 
# 
# diff2 <- compare_df(tax_table, rt_aug_file, c("taxon_id"))
# 

################################################

# Use another method to Diff the two files:

tax_table[!(tax_table$taxon_id %in% rt_aug_file$taxon_id),]   # Finally something that's useful!!

anti_join(tax_table, rt_aug_file, by = "taxon_id")  # returns same as line above

diff3 <- anti_join(tax_table, rt_aug_file)  # returns 989 differences

