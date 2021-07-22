# Load packages
library(qualtRics)
library(tidyverse)
library(janitor)
library(ggplot2)
library(grid)
library(cowplot)
library(httr)
library(extrafont)
library(here)
library(Hmisc)

# Read and clean data from Qualtrics-----

# read in 1B data from csv

data1B <- read_csv(here("data_files", "2021_07_03-data1B.csv"))

# use names() to get the variable numbers you want to select

names(data1B)

#filter data1B to only include survey responses not survey previews and select just variables of interest
data1B_select <- data1B %>%
  filter(status == "IP Address") %>%
  select(q2:q50)

# Rename data variables-----

# let's rename the variables

data1B_rename <- data1B_select %>%
  rename(coder_name = q2, coder_email = q3, no_of_experiments = q51, type_of_article = q52, type_of_article_other = q52_2_text, participants = q5, 
         participants_other = q5_3_text, age = q6, brain_beh = q7, topic = q8, topic_other = q8_5_text, 
         
         data_badge = q9, data_statement_present = q10, data_statement_indicates = q11, data_quote = q12, data_page_no = q13, 
         data_accessible = q14, data_accessible_other = q14_4_text, dataset_URL = q15, type_of_data_repo = q16, type_of_data_repo_other = q16_3_text, 
         which_data_repo = q17, which_data_repo_other = q17_6_text, dataset_URL_working = q18, data_locatable = q19, data_downloadable = q20, data_correspond = q21, 
         data_complete = q22, software = q23, type_of_software = q24, type_of_software_other = q24_8_text, data_codebook = q25, data_scripts = q26, data_other_info = q27, 
         
         materials_badge = q28, materials_statement_present = q29, materials_statement_indicates = q30, materials_quote = q31, materials_page_no = q32, 
         materials_accessible = q33, materials_accessible_other = q33_4_text, materials_URL = q34, type_of_materials_repo = q35, type_of_materials_repo_other = q35_3_text, 
         which_materials_repo = q36, which_materials_repo_other = q36_6_text, materials_URL_working = q37, materials_locatable = q38, materials_downloadable = q39, materials_correspond = q40, 
         materials_complete = q41, materials_explanation = q42, materials_other_info = q43, supplemental_info = q49, URL_supplemental_info = q50) 

# separate article id number and journal code and filter OUT the articles that were reliability checking 
data1B_sep <- data1B_rename %>%
  separate(q4_1, into = c("article_id_number"), sep = "\\s", remove = FALSE) %>%
  filter(!str_detect(q4_1,'Check')) %>%
  rename(article_id_number_and_title = q4_1)

# writing a csv. file containing ONLY reliability check articles
data1B_reliability_checking <- data1B_rename %>%
  separate(q4_1, into = c("article_id_number"), sep = "\\s", remove = FALSE) %>%
  filter(str_detect(q4_1,'Check')) %>%
  rename(article_id_number_and_title = q4_1)

data1B_reliability_checking %>% write_csv(here::here("data_files", "data1B_reliability_checking"))

# Filter out unwanted duplicates----

# let's check whether there are any articles which have been coded more than once
duplicates <- get_dupes(data1B_sep, article_id_number) 

# to solve the duplicate problem, CR recoded all the duplicates (except for one, which she had coded originally), here filtering for her 4 recoded observations
dup_recoded <- duplicates %>%
  filter(coder_name == "Christina Rochios") %>%
  select(-dupe_count) 

# let's remove all duplicated rows from the clean data1B dataset using the distinct() function
data1B_distinct <- data1B_sep %>%
  distinct(article_id_number, .keep_all = TRUE) 
# this leaves us with 242 obs - so it seems like the distinct() function leaves the first version of each duplicate in the dataframe
# BUT we want to deleted ALL versions of the duplicates

# so let's filter out the rows we know are duplicates

# first let's create a dataframe with all the article ids we know are duplicates
dups <- c("2019-30-10-1424", "2019-30-7-989", "2020-31-2-214", "2020-31-3-243", "2020-31-9-1084")

# then let's delete these known duplicates using the %nin% operator from the Hmisc pacakge
data1B_nodups <- data1B_distinct %>%
  filter(article_id_number %nin% dups) # yes goes from 242 to 237 - removed 5 duplicates 

# now let's add back the versions of the duplicates we want (i.e. those in dup_recoded)
master_dups <- rbind(data1B_nodups, dup_recoded) 
# Awesome - we've ended up with 242 obs again!

# let's write this dataset into a .csv file
master_dups %>% write_csv(here::here("data_files", "master_dataset_1B.csv"))