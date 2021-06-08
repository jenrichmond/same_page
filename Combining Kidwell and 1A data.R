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

# First let's read the Kidwell data
kidwell <- read_csv(here("data_files", "master_dataset.csv")) %>%
  clean_names()

# Now lets clean the names of articles and dates
kidwell_clean <- kidwell %>%
  mutate(journal = case_when(
    str_detect(article_id_number,'CPS') ~ "Clinical Psychological Science",
    str_detect(article_id_number,'DP') ~ "Developmental Psychology",
    str_detect(article_id_number,'JEPLMC') ~ "Journal of Experimental Psychology: Learning, Memory, and Cognition",
    str_detect(article_id_number,'JPSP') ~ "Journal of Personality and Social Psychology",
    str_detect(article_id_number,'PS') ~ "Psychological Science",  TRUE ~ "other")) %>%
  mutate(year = case_when(str_detect(article_id_number,'2012') ~ "2012",
                          str_detect(article_id_number,'2013') ~ "2013",
                          str_detect(article_id_number,'2014') ~ "2014",
                          str_detect(article_id_number,'2015') ~ "2015",
                          TRUE ~ "other")) %>%
  relocate(journal, .after = article_id_number) %>%
  relocate(year, .before = article_id_number)

# Filter the relevant data (i.e. only articles published in 2014 and 2015)
relevant_kidwell_clean <- kidwell_clean %>%
  filter(year %in% c("2014", "2015")) %>%
  filter(journal %in% c("Psychological Science"))

# separate article_id_number variable into ID and journal
relevant_kidwell_clean <- relevant_kidwell_clean %>%
  separate(article_id_number, into = c("article_id_number", "journal"), sep = "\\s", remove = TRUE)
  
# Import 1A data from qualtrics 

qualtrics_api_credentials(api_key = "RZNCuPM7MvUJmhi7wcSLN7NHx0WCVeIU6H7XiSeE", 
                          base_url = "unsw.syd1.qualtrics.com",
                          install = TRUE, 
                          overwrite = TRUE)

# Replace {  } with the datacenter ID from Account Setting = syd1
# https://{datacenterid}.qualtrics.com/API/V3/pathToRequest
# https://syd1.qualtrics.com/API/V3/pathToRequest

readRenviron("~/.Renviron")  

# Pull all the surveys from qualtrics and get a list of survey IDs
surveys <- all_surveys()
surveys$id

# Read in the questions and survey data from the 1A survey
  # 1A ID = SV_0GSSTcRvY9x0x4q

# pull 1A questions
questions <- survey_questions(surveyID = "SV_0GSSTcRvY9x0x4q")

# pull 1A data
data1A  <- fetch_survey(surveyID = "SV_0GSSTcRvY9x0x4q", verbose = TRUE,force_request = TRUE) %>%
  clean_names()

#filter data1A to only include survey responses not survey previews
data1A <- data1A %>%
  filter(status == "IP Address") 

# delete unnecessary variables/columns
data1A = data1A[,!(names(data1A) %in% c("start_date", "end_date", "status", "ip_address", "progress", "duration_in_seconds", "finished", "recorded_date", "response_id", "recipient_last_name", "recipient_first_name", "recipient_email", "external_reference", "location_latitude", "location_longitude", "distribution_channel", "user_language", "q10_8_text_parent_topics", "q10_8_text_sentiment_polarity", "q10_8_text_sentiment_score", "q10_8_text_sentiment", "q10_8_text_topic_sentiment_label", "q10_8_text_topic_sentiment_score", "q10_8_text_topics"))]

# let's rename the variables
data1A_rename <- data1A %>%
  rename(coder_name = q2, coder_email = q3, no_of_experiments = q31, type_of_article = q33, type_of_article_other = q33_7_text, participants = q5, participants_other = q5_3_text, 
         age = q6, brain_beh = q7, topic = q8, topic_other = q8_5_text, software = q9, type_of_software = q10, type_of_software_other = q10_8_text) 
# need Jenny's help here - for some reason this renaming isn't working 

# split the article ID column into ID, journal and title 
data1A_rename <- data1A %>%
  separate(q4_1, into = c("article_id_number", "journal", "article_title"), sep = "\\s", remove = TRUE)
# need Jenny's help here - the title variable only has the first word of the title instead of the whole title

# merge data1A_rename dataset and relevant_kidwell_clean dataset
master_1A_dataset <- merge(data1A_rename, relevant_kidwell_clean, by="article_id_number")

# clean the master dataset so it doesn't contain replicated information
# question for Jenny: do we want to use this double-up information to check reliability or anything like that?
master_1A_dataset_clean = master_1A_dataset[,!(names(master_1A_dataset) %in% c("timestamp", "your_name", "if_other", "year", "journal.y", "number_of_experiments"))]
  
