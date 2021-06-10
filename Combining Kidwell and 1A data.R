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
  
# read in 1A data from csv

data1A <- read_csv(here("data_files", "data1A.csv"))

# use names() to get the variable numbers you want to select

names(data1A)

#filter data1A to only include survey responses not survey previews and select just variables of interest
data1A_select <- data1A %>%
  filter(status == "IP Address") %>%
  select(q2:q10_8_text)

# delete unnecessary variables/columns
# JR: the code below is A LOT :) to just select the 15 variables in the middle, using select and the range of variables (q2:q10_8_text OR 18:31)  might be easier... you can also pipe that on the end of your filter ABOVE and do it in one step

data1A = data1A[,!(names(data1A) %in% c("start_date", "end_date", "status", "ip_address", "progress", "duration_in_seconds", "finished", "recorded_date", "response_id", "recipient_last_name", "recipient_first_name", "recipient_email", "external_reference", "location_latitude", "location_longitude", "distribution_channel", "user_language", "q10_8_text_parent_topics", "q10_8_text_sentiment_polarity", "q10_8_text_sentiment_score", "q10_8_text_sentiment", "q10_8_text_topic_sentiment_label", "q10_8_text_topic_sentiment_score", "q10_8_text_topics"))]


# let's rename the variables
# JR ok print the names first to make sure you are matching

names(data1A_select)

data1A_rename <- data1A_select %>%
  rename(coder_name = q2, coder_email = q3, no_of_experiments = q31, type_of_article = q33,         type_of_article_other = q33_7_text, participants = q5, 
         participants_other = q5_3_text, age = q6, brain_beh = q7, topic = q8, 
         topic_other = q8_5_text, software = q9, type_of_software = q10,       type_of_software_other = q10_8_text) 

names(data1A_rename)
# need Jenny's help here - for some reason this renaming isn't working 
# JR dont know why that wasn't working, I just started out renaming a few at a time and then strung them all together and it works now

# split the article ID column into ID, journal and title 

#JR you want to be careful that you are working with the output of the last step. Here you were starting with the data1A, when the output of the last step was already data1A_rename. 

data1A_sep <- data1A_rename %>%
  separate(q4_1, into = c("article_id_number", "journal"), sep = "\\s", remove = FALSE)
# need Jenny's help here - the title variable only has the first word of the title instead of the whole title
# JR ahhhh this is because it is separating on space, do we need to whole title, maybe it is ok to pull the id number and journal and use remove = FALSE so we have the whole title still in q4_1 if we need it for something? Ive dropped the 3rd components of the into = so the title stays in Q4_1

----------
# I think we can delete this code/comments and replace it with what's below:
  
# merge data1A_rename dataset and relevant_kidwell_clean dataset

master_1A_dataset <- merge(data1A_sep, relevant_kidwell_clean, by="article_id_number")

# clean the master dataset so it doesn't contain replicated information
# question for Jenny: do we want to use this double-up information to check reliability or anything like that?


# JR re keeping for reliability, i think because it would throw errors if it couldn't match article id numbers, I think you can assume it hasn't stuff up the join. I wonder whether the join functions from dplyr https://dplyr.tidyverse.org/reference/join.html might automatically get rid of duplicates as part of the joining process?? 


# JR as above, i think your code is cleaner and more understandble if you use select to keep the variables you want, rather than dropping the variables you don't via indexing. 

master_1A_dataset_clean = master_1A_dataset[,!(names(master_1A_dataset) %in% c("timestamp", "your_name", "if_other", "year", "journal.y", "number_of_experiments"))]

------------
  
# join data1A_sep and relevant_kidwell_clean datasets by article ID

master_1A_dataset <- full_join(data1A_sep, relevant_kidwell_clean, by="article_id_number")
# CR I researched the join function to see whether it could automatically take out the duplicated variables, but all I came across was a function that removed duplicated rows instead of columns

# clean the master dataset so that duplicated and unwanted columns/variables are removed
master_1A_dataset_clean <- master_1A_dataset %>%
  select(coder_name:type_of_software_other, did_the_article_receive_a_badge_for_open_data:corresponding_author_e_mail_address) 

# let's check that we will have the same number of empirical articles as Kidwell et al. 
master_1A_dataset_clean %>%
  count(no_of_experiments %in% c("1", "2", "3", "4", "5_or_more"))
# We coded there to be 333 empirical articles and 71 non-empirical articles
# also - not sure if I'm using the correct function to count the number of variables here?

# To check the empirical vs. non-empirical count for Kidwell, let's delete our no_of_experiments variable rather than the no_of_experiments variable from the Kidwell study
master_1A_dataset_clean_check <- master_1A_dataset %>%
  select(coder_name:journal.x, type_of_article:type_of_software_other, number_of_experiments:corresponding_author_e_mail_address) %>%
  count(number_of_experiments > 0)
# Kidwell coded there to be 334 empirical articles, 47 non-empirical articles and 23 NA articles

# I don't know what the 23 NA articles from Kidwell are but there appears to be only 1 discrepancy between our data and their data (woohoo!!)
# Question for Jenny: is there a function we can use to locate where this discrepancy is?

# write master_1A_dataset_clean to csv

master_1A_dataset_clean %>% write_csv(here::here("data_files", "master_dataset_1A"))
