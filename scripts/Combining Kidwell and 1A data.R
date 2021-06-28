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

# Read and clean Kidwell data-----

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

# Read and clean Qualtrics 1A data------
  
# read in 1A data from csv

data1A <- read_csv(here("data_files", "2021_06_28-data1A.csv"))

# use names() to get the variable numbers you want to select

names(data1A)

#filter data1A to only include survey responses not survey previews and select just variables of interest
data1A_select <- data1A %>%
  filter(status == "IP Address") %>%
  select(start_date, q2:q10_8_text)

# let's rename the variables
data1A_rename <- data1A_select %>%
  rename(coder_name = q2, coder_email = q3, no_of_experiments = q31, type_of_article = q33,         type_of_article_other = q33_7_text, participants = q5, 
         participants_other = q5_3_text, age = q6, brain_beh = q7, topic = q8, 
         topic_other = q8_5_text, software = q9, type_of_software = q10,       type_of_software_other = q10_8_text) 

# separate article id number and journal code and filter OUT the articles that were reliability checking 
data1A_sep <- data1A_rename %>%
  separate(q4_1, into = c("article_id_number", "journal_code"), sep = "\\s", remove = FALSE) %>%
  filter(!str_detect(q4_1,'Check'))

# Remove duplicated rows from Qualtrics data----

# let's create a dataframe with all the duplicated rows, according to article id number
duplicates <- get_dupes(data1A_sep, article_id_number) 

# to solve duplicate problem, CR recoded all the duplicates, here filtering for her 18 recoded observations
dup_recoded <- duplicates %>%
  filter(coder_name == "Christina Rochios") %>%
  filter(no_of_experiments != 0) %>% # filter out the one that CR coded twice
  select(-dupe_count)

# let's remove all duplicated rows from the clean data1A dataset using the unique() function
data1A_duplicates_removed <- unique(data1A_sep)
  # this leaves us with 400 obs

# let's check this using the distinct() function
data1A_distinct <- data1A_sep %>%
  distinct(article_id_number, .keep_all = TRUE) 
  # this leaves us with 367 obs - so it seems like the distinct() function leaves the first version of each duplicate in the dataframe
  # BUT we want to deleted ALL versions of the duplicates

# so let's filter out the rows we know are duplicates
data1A_distinct %>%
  filter(data1A_distinct, article_id_number != "1-5-2014", "10-2-2014", "11-2-2014", "12-12-2014", "12-8-2014", "13-4-2014", "18-12-2014", "19-12-2014", "19-2-2014", "2-1-2015", "24-3-2014", "24-4-2015", "24-7-2014", "3-3-2014", "4-6-2014", "6-3-2015", "7-8-2014", "8-3-2015", "9-2-2014")
  # I don't know why but this isn't working - need Jenny's help here

# now let's add back the versions of the duplicates we want (i.e. those in dup_recoded)
master_dups <- rbind(data1A_distinct, dup_recoded) 
# if this has worked properly we should end up with 367 obs again

# OK now we're ready to merge our data with Kidwell's

# Combining Kidwell and 1A data----

# Where Christina is up to




# merge data1A_rename (400obs) dataset and relevant_kidwell_clean (367obs) dataset to get master1Adataset (400obs)

master_1A_dataset <- merge(data1A_sep, relevant_kidwell_clean, by="article_id_number")
  
# COMPARING NO and NUM of experiments----------------

# change no of experiments to numeric

master_1A_dataset$no_of_experiments <- as.numeric(master_1A_dataset$no_of_experiments)

# make a new variable to make the 1A coding and kidwell number of experiment in consistent format (char)
# no_of_experiments is our coding, num_of_experiences is kidwell coding in character format
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


# working out obs difference-----------------

# Christina investigating why the number of observations is different for 'master_1A_dataset' and 'relevant_kidwell_clean'

# let's check whether there are duplicates in article_id_number column

duplicates <- get_dupes(master_1A_dataset, article_id_number) 

duplicates %>%
  write_csv(here("data_files", "2021-06-21_duplicates1A.csv"))

# to solve duplicate problem, CR recoded all the duplicates, here filtering for her 18 recoded observations

dup_recoded <- duplicates %>%
  filter(coder_name == "Christina Rochios") %>%
  filter(no_of_experiments != 0) %>% # filter out the one that CR coded twice
  select(-dupe_count)

# now rbind dup_recoded onto the master1A and then use distinct() to keep the most recent obs of duplicated articleid_numbers, problem with the distinct function is that it picks 1 of the dups to keep (but not the right one!)

# master + dups = 418 observations
master_dups <- rbind(master_1A_dataset, dup_recoded) 

distinct_master <- master_dups %>% 
  distinct(article_id_number, .keep_all= TRUE)

dups <- distinct_master %>%
  get_dupes(article_id_number)

# we want to filter OUT obs that are in this dup_list so that we can add them back in
dup_list <- dup_recoded$article_id_number



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
