# Preparing 1A Reliability Check Articles

library(qualtRics)
library(tidyverse)
library(janitor)

# Import 1A reliability data from qualtrics 

qualtrics_api_credentials(api_key = "x6zwQgKWTkduv783BnlSXfwam8yGGYLRpEcZrAnF", 
                          base_url = "unsw.syd1.qualtrics.com",
                          install = TRUE, 
                          overwrite = TRUE)

readRenviron("~/.Renviron")  

reliability1A  <- fetch_survey(surveyID = "SV_1NVNjqwtoR2vfV4", verbose = TRUE,force_request = TRUE) %>%
  clean_names()

#filter to select only variables of interest
select1A <- reliability1A  %>%
  select(q2:q10_8_text)

# let's rename the variables
rename1A <- select1A %>%
  rename(coder_name = q2, coder_email = q3, no_of_experiments = q31, type_of_article = q33,         type_of_article_other = q33_7_text, participants = q5, 
         participants_other = q5_3_text, age = q6, brain_beh = q7, topic = q8, 
         topic_other = q8_5_text, software = q9, type_of_software = q10,       type_of_software_other = q10_8_text) 

# separate article id number and journal code
sep1A <- rename1A %>%
  separate(q4_1, into = c("article_id_number", "check_id"), sep = "\\s", remove = FALSE) %>%
  rename(article_id_number_and_title = q4_1) %>%
  mutate(article_id_number = str_remove_all(article_id_number, "\""))

# write a .csv file that only contains Reliability Check articles
sep1A %>% write_csv(here::here("data_files", "data1A_reliability_checking"))





