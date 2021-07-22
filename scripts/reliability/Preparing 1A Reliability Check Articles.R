# Preparing 1A Reliability Check Articles

library(qualtRics)
library(tidyverse)
library(janitor)

# Import 1A data from qualtrics 

qualtrics_api_credentials(api_key = "RZNCuPM7MvUJmhi7wcSLN7NHx0WCVeIU6H7XiSeE", 
                          base_url = "unsw.syd1.qualtrics.com",
                          install = TRUE, 
                          overwrite = TRUE)

readRenviron("~/.Renviron")  

# pull 1A data
data1A  <- fetch_survey(surveyID = "SV_0GSSTcRvY9x0x4q", verbose = TRUE,force_request = TRUE) %>%
  clean_names()

# write 1A data to csv

data1A %>% write_csv(here::here("data_files", "2021_07_22-data1A.csv"))

#filter data1A to only include 'Check' articles and select just variables of interest
data1A_select <- data1A %>%
  filter(str_detect(q4_1, 'Check')) %>%
  select(q2:q10_8_text)

# let's rename the variables
data1A_rename <- data1A_select %>%
  rename(coder_name = q2, coder_email = q3, no_of_experiments = q31, type_of_article = q33,         type_of_article_other = q33_7_text, participants = q5, 
         participants_other = q5_3_text, age = q6, brain_beh = q7, topic = q8, 
         topic_other = q8_5_text, software = q9, type_of_software = q10,       type_of_software_other = q10_8_text) 

# separate article id number and journal code
data1A_sep <- data1A_rename %>%
  separate(q4_1, into = c("type_of_article", "check_id"), sep = "\\s", remove = FALSE) %>%
  rename(article_id_number_and_title = q4_1)

# remove any duplicated rows
data1A_clean <- data1A_sep %>%
  distinct()

# write a .csv file that only contains Reliability Check articles
data1A_clean %>% write_csv(here::here("data_files", "data1A_reliability_checking"))





