# Final 1B Preparation Before Analysis 

# After we ran the 'Assigning and Scoring' script for 1B for the first time (16/07/21) we noticed that there were a few articles which had received a data/materials badge but had received a very low score

# Christina investigated these articles and found that in most cases, the articles had been coded incorrectly

  # Data

    # 2019-30-7-1001 - incorrectly coded (data are available)
    # 2019-30-9-1362 - incorrectly coded (data are available)
    # 2020-31-2-193 - not sure (the data is only available upon request, but the analysis scripts are publicly available)
    # 2020-31-7-881 - not sure (the data is only available to qualified researchers, but the R code is available in the supplementary materials)
    # 2020-31-8-927 - incorrectly coded (data are available)
    # 2019-30-8-1218 - correctly coded (data is not locatable)

  # Materials

    # 2019-30-11-1561 - incorrectly coded (materials are available)
    # 2019-30-7-1001 - incorrectly coded (materials are available)
    # 2019-30-7-1016 - incorrectly coded (materials are available)
    # 2020-31-10-1236 - incorrectly coded (materials are available)
    # 2020-31-3-306 - incorrectly coded (materials are available)
    # 2020-31-5-488 - incorrectly coded (materials are available)
    # 2020-31-8-1013 - incorrectly coded (materials statement available, not sure whether actual materials are available)
    # 2020-31-8-1025 - incorrectly coded (materials are available)
    # 2020-31-9-1129 - incorrectly coded (materials are available)

# Surprisingly, some articles which didn't receive a data/materials badge receieved a very high score

# Christina investigated these cases and found that whilst they didn't receive a badge, they had open data/materials 

# We concluded that perhaps the authors of these articles elected not to be awarded a badge

# Study 1A

  # Data

    # 23-2-2014 - correctly coded 

  # Materials

    # 23-2-2014 - SAME ARTICLE AS ABOVE correctly coded 

# Study 1B
  
  # Data

    # 2019-30-7-1016 - correctly coded 
    # 2020-31-6-634 - correctly coded 

  # Materials
  
    # None

# On 06/08/21 Christina noticed that one of the coders had incorrectly coded 8 articles as non-empirical

  # 2019-30-9-1403
  # 2020-31-7-890 
  # 2020-31-10-1340
  # 2020-31-8-1036
  # 2020-31-8-1040 
  # 2020-31-3-338
  # 2020-31-6-748
  # 2020-31-6-741

# In all 19 cases where the article had been incorrectly coded in some way, Christina recoded the article

# This script combines the 19 recoded 1B articles with the data in the master_dataset_1B.csv

-------------

library(qualtRics)
library(tidyverse)
library(janitor)
library(here)
library(Hmisc)

# Import 1B data from qualtrics that was coded after 20/07/21-------
  # 20/07 onwards was when Christina recoded the incorrectly coded articles

qualtrics_api_credentials(api_key = "RZNCuPM7MvUJmhi7wcSLN7NHx0WCVeIU6H7XiSeE", 
                          base_url = "unsw.syd1.qualtrics.com",
                          install = TRUE, 
                          overwrite = TRUE)

readRenviron("~/.Renviron")  

# pull 1B data
recoded_articles  <- fetch_survey(surveyID = "SV_86Um9YiXUcH6eGy", start_date = "2021-07-20", verbose = TRUE,force_request = TRUE) %>%
  clean_names()

#filter data1B to only include variables of interest
relevant_data <- recoded_articles %>%
  select(q2:q50)

# Rename data variables-----

# let's rename the variables

renamed_data <- relevant_data %>%
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

# separate article id number and journal code 
renamed_data <- renamed_data %>%
  separate(q4_1, into = c("article_id_number"), sep = "\\s", remove = FALSE) %>%
  rename(article_id_number_and_title = q4_1)

# Import existing 1B master dataset-----

data1B <- read_csv(here("data_files", "cleaned_dataset_1B.csv"))

# Add newly coded articles to existing articles-----

all_data <- rbind(data1B, renamed_data)

# Filter out unwanted duplicates----

# let's check that the only duplicates are the recoded articles 
duplicates <- get_dupes(all_data, article_id_number) 

# let's filter for Christina's recoded versions of the duplicated articles
dup_recoded <- duplicates %>%
  filter(coder_name == "Christina Rochios") %>%
  select(-dupe_count) 

# let's remove ALL the duplicated rows (both newly coded and previously coded) from all_data

# first let's create a dataframe with all the article ids we know are duplicates
dups <- c("2019-30-11-1561", "2019-30-7-1001", "2019-30-7-1016", "2019-30-9-1362", "2020-31-10-1236", "2020-31-3-306", "2020-31-5-488", "2020-31-8-1013", "2020-31-8-1025", "2020-31-8-927", "2020-31-9-1129", "2019-30-9-1403", "2020-31-7-890", "2020-31-10-1340", "2020-31-8-1036", "2020-31-8-1040", "2020-31-3-338", "2020-31-6-748", "2020-31-6-741")

# then let's delete these known duplicates using the %nin% operator from the Hmisc pacakge
data1B_nodups <- all_data %>%
  filter(article_id_number %nin% dups) 

# now let's add back the versions of the duplicates we want (i.e. those in dup_recoded)
master_dups <- rbind(data1B_nodups, dup_recoded) 

# Write .csv-----

# let's write this dataset into a .csv file
master_dups %>% write_csv(here::here("data_files", "master_dataset_1B.csv"))



