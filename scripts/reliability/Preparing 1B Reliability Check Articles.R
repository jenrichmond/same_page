# Preparing 1B Reliability Check Articles

library(qualtRics)
library(tidyverse)
library(janitor)

# Import 1B data from qualtrics 

qualtrics_api_credentials(api_key = "RZNCuPM7MvUJmhi7wcSLN7NHx0WCVeIU6H7XiSeE", 
                          base_url = "unsw.syd1.qualtrics.com",
                          install = TRUE, 
                          overwrite = TRUE)

readRenviron("~/.Renviron")  

# pull 1B data
data1B  <- fetch_survey(surveyID = "SV_86Um9YiXUcH6eGy", verbose = TRUE,force_request = TRUE) %>%
  clean_names()

# write 1B data to csv

data1B %>% write_csv(here::here("data_files", "2021_07_22-data1B.csv"))

#filter data1B to only include 'Check' articles and select just variables of interest
data1B_select <- data1B %>%
  filter(str_detect(q4_1, 'Check')) %>%
  select(q2:q50)

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

# separate article id number and journal code
data1B_sep <- data1B_rename %>%
  separate(q4_1, into = c("type_of_article", "check_id"), sep = "\\s", remove = FALSE) %>%
  rename(article_id_number_and_title = q4_1)

# remove any duplicated rows
data1B_clean <- data1B_sep %>%
  distinct()

# write a .csv file that only contains Reliability Check articles
data1B_clean %>% write_csv(here::here("data_files", "data1B_reliability_checking"))
