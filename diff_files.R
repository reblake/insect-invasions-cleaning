#################
###  Diff between Rebecca Turner's taxonomy file and the authoritative taxonomy table
################

library(tidyverse) ; library(testthat)

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

diff1 <- setdiff(tax_table, rt_aug_file)
str(diff1)

################################################

# Use another method to Diff the two files:

tax_table[!(tax_table$taxon_id %in% rt_aug_file$taxon_id),]   # something that's useful!!

anti_join(tax_table, rt_aug_file, by = "taxon_id")  # returns same as line above

diff3 <- anti_join(tax_table, rt_aug_file)  # returns differences
str(diff3)

#####
# I suspect that taxa are being numbered differently between the two cleaning codes, so testing that here
tax_table2 <- tax_table %>% mutate(id_usr_supp_nm = paste(taxon_id, user_supplied_name))

rt_aug_file2 <- rt_aug_file %>% mutate(id_usr_supp_nm = paste(taxon_id, user_supplied_name))

# compare based on the two pasted columns
diff4 <- anti_join(tax_table2, rt_aug_file2, by = "id_usr_supp_nm") # rows of tax_table2 that don't have a match in rt_aug_file2
str(diff4)

diff5 <- anti_join(rt_aug_file2, tax_table2, by = "id_usr_supp_nm") # rows of rt_aug_file2 that don't have a match in tax_table2
str(diff5)

#####
# Subset diff3 to remove rows with taxon_id mismatches
other_probs <- anti_join(diff3, diff4) %>% arrange(user_supplied_name) %>% 
               

usr_nms_diff <- other_probs$user_supplied_name

rt_diff <- rt_aug_file %>% filter(user_supplied_name %in% usr_nms_diff) %>% 
           arrange(user_supplied_name)

# This is actually quite useful!  Gives a print out of which columns have issues and how many issues.
testthat::expect_equal(other_probs, rt_diff)   

# further investigation based on testthat results
tt_status_na <- other_probs[is.na(other_probs$status), ]
rt_status_na <- rt_diff[is.na(rt_diff$status), ]
anti_join(tt_status_na, rt_status_na, by = "user_supplied_name")


