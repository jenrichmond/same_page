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
  separate(article_id_number, into = c("article_id_number", "journal_code"), sep = "\\s", remove = TRUE)
  
# read in 1A data from csv

data1A <- read_csv(here("data_files", "data1A.csv"))

# use names() to get the variable numbers you want to select

names(data1A)

#filter data1A to only include survey responses not survey previews and select just variables of interest
data1A_select <- data1A %>%
  filter(status == "IP Address") %>%
  select(q2:q10_8_text)


# let's rename the variables

data1A_rename <- data1A_select %>%
  rename(coder_name = q2, coder_email = q3, no_of_experiments = q31, type_of_article = q33,         type_of_article_other = q33_7_text, participants = q5, 
         participants_other = q5_3_text, age = q6, brain_beh = q7, topic = q8, 
         topic_other = q8_5_text, software = q9, type_of_software = q10,       type_of_software_other = q10_8_text) 

# separate article id number and journal code and filter OUT the articles that were reliability checking 
data1A_sep <- data1A_rename %>%
  separate(q4_1, into = c("article_id_number", "journal_code"), sep = "\\s", remove = FALSE) %>%
  filter(!str_detect(q4_1,'Check'))


# merge data1A_rename dataset and relevant_kidwell_clean dataset

master_1A_dataset <- merge(data1A_sep, relevant_kidwell_clean, by="article_id_number")
  

# change no of experiments to numeric

master_1A_dataset$no_of_experiments <- as.numeric(master_1A_dataset$no_of_experiments)

# make a new variable to make the 1A coding and kidwell number of experiment in consistent format (char)
master_1A_dataset <- master_1A_dataset %>%
  mutate(num_of_experiments = case_when(number_of_experiments >= 5 ~ "5 or more", 
                                     number_of_experiments == 4 ~  "4", 
                                     number_of_experiments == 3 ~  "3",
                                     number_of_experiments == 2 ~  "2",
                                     number_of_experiments == 1 ~  "1",
                                     number_of_experiments == 0 ~  "0"))
                                     
 # mutate new variable that checks whether no (1A) and num (kidwell) of experiments is the same                                       
master_1A_dataset <- master_1A_dataset %>%
  mutate(exp_check = case_when(no_of_experiments == num_of_experiments ~ "TRUE", 
                               no_of_experiments != num_of_experiments ~ "FALSE"))

# filter only cases where it is NOT the same 
exp_check <- master_1A_dataset %>%
  filter(exp_check == FALSE) %>%
  relocate(num_of_experiments, .after = no_of_experiments)

# Christina investigating why the number of observations is different for 'master_1A_dataset' and 'relevant_kidwell_clean'

# let's check whether there are duplicates in article_id_number column
duplicates <- get_dupes(master_1A_dataset, article_id_number)

duplicates %>%
  write_csv(here("data_files", "2021-06-21_duplicates1A.csv"))

# so there are 28 duplicated observations in the article_id_number column, which means there are 14 articles that have been coded twice

# let's try and figure out which articles are present in our data1A_sep dataset that AREN'T present in relevant_kidwell_clean and vice versa

# LETS use anti_join, first lets make the problem smaller by select just the article IDs from each dataset

kidwell_IDs <- relevant_kidwell_clean %>%
  select(article_id_number)

data1A_IDs <- data1A_sep %>%
  select(article_id_number)

# antijoin, give me all the IDs that are in kidwell but not in data1A
mismatch1 <- anti_join(kidwell_IDs, data1A_IDs, by = "article_id_number")

# so there are 4 articles which Kidwell coded that we didn't

# antijoin, give me all the IDs that are in data1A but not in kidwell 
mismatch2 <- anti_join(data1A_IDs, kidwell_IDs, by = "article_id_number")

# Kidwell coded all the articles we coded


# OK this might actually make sense now 
  # there are 10 extra observations in our dataset (data1A_sep) compared to Kidwell 
  # there are 14 article ID duplicates in the master dataset
  # we've just figured out that there are 4 uncoded for articles in our dataset (data1A_sep)

# Next steps
  # code 4 articles which haven't been coded - 21/06 Update: DONE
  # figure out why there 14 duplicates and delete the versions which are most inaccurate - 21/06 Update: DONE

# LETS remove the duplicated rows so that we keep only the most recent instance of each row
master_1A_dataset_duplicates_removed <- master_1A_dataset %>%
  !duplicated[("article_id_number"), fromLast=T]

master_1A_dataset_duplicates_removed <- master_1A_dataset %>%
  master_1A_dataset[!duplicated(master_1A_dataset$article_id_number, fromLast=T)]

# CR needs Jenny's help here!

# clean the master dataset so that duplicated and unwanted columns/variables are removed
master_1A_dataset_clean <- master_1A_dataset %>%
  select(article_id_number:type_of_software_other, number_of_experiments:corresponding_author_e_mail_address) 

# let's check that we will have the same number of empirical articles as Kidwell et al. 
master_1A_dataset_clean %>%
  count(no_of_experiments %in% c("1", "2", "3", "4", "5_or_more"))

master_1A_dataset_clean %>%
  count(number_of_experiments > 0)


### need to fix duplicates and code extra 4 articles before running this again to check that we have hte same number of empirical articles across kidwell and 1A 

# JENNY AND CHRISTINA UP TO HERE

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
