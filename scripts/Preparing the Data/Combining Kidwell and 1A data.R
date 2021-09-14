# Load packages

library(tidyverse)
library(janitor)
library(here)
library(Hmisc)

# Read and clean Kidwell et al. (2016) data-----

# Reading in Kidwell et al.'s master dataset (which we downloaded from their OSF: https://osf.io/a29bt/)
kidwell <- read_csv(here("data_files", "kidwell_et_al._(2016)_master_dataset.csv")) %>%
  clean_names()
  # Note: you will need to change the location of the file, depending on where you have saved it

# Pull the journal name and year of publication out from the article ID numbers 
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
  relocate(year, .before = article_id_number)  # moves new journal and year columns

# Filter the relevant data (i.e. only PsychSci articles published in 2014 and 2015)
relevant_kidwell_clean <- kidwell_clean %>%
  filter(year %in% c("2014", "2015")) %>%
  filter(journal %in% c("Psychological Science"))

# Separate article_id_number variable into ID and journal code
relevant_kidwell_clean <- relevant_kidwell_clean %>%
  separate(article_id_number, into = c("article_id_number", "journal_code"), sep = "\\s", remove = TRUE)

# Read and clean data from Qualtrics ------
  
# Read in 1A data from csv

data1A <- read_csv(here("data_files", "Study 1A Qualtrics Data.csv"))

# Filter data to only include survey responses and select just variables of interest
data1A_select <- data1A %>%
  filter(status == "IP Address") %>%
  select(q2:q10_8_text)

# Rename the variables
data1A_rename <- data1A_select %>%
  rename(coder_name = q2, coder_email = q3, no_of_experiments = q31, type_of_article = q33,         type_of_article_other = q33_7_text, participants = q5, 
         participants_other = q5_3_text, age = q6, brain_beh = q7, topic = q8, 
         topic_other = q8_5_text, software = q9, type_of_software = q10,       type_of_software_other = q10_8_text) 

# Separate article id number and journal code and filter OUT the observations that were reliability checking 
data1A_sep <- data1A_rename %>%
  separate(q4_1, into = c("article_id_number", "journal_code"), sep = "\\s", remove = FALSE) %>%
  filter(!str_detect(q4_1,'Check')) %>%
  rename(article_id_number_and_title = q4_1)

# Remove duplicated rows from Qualtrics data----

# Create a dataframe with all the duplicated rows, according to article id number
duplicates <- get_dupes(data1A_sep, article_id_number) 

# To solve the duplicate problem, the senior coder recoded all the duplicates, here filtering for her 19 recoded observations
dup_recoded <- duplicates %>%
  filter(coder_name == "Christina Rochios") %>%
  filter(no_of_experiments != 0) %>% # filter out the one that the senior coder coded twice
  select(-dupe_count) 

# Remove all duplicated rows from the clean data1A dataset using the distinct() function
data1A_distinct <- data1A_sep %>%
  distinct(article_id_number, .keep_all = TRUE) 
  # This leaves the first version of each duplicate in the dataframe

# To delete ALL versions of the duplicates, we filtered out the rows we knew were duplicates

# Create a dataframe with all the article ids we know are duplicates
dups <- c("1-5-2014", "10-2-2014", "11-2-2014", "12-12-2014", "12-8-2014", "13-4-2014", "18-12-2014", "19-12-2014", "19-2-2014", "2-1-2015", "24-3-2014", "24-4-2015", "24-7-2014", "3-3-2014", "4-6-2014", "6-3-2015", "7-8-2014", "8-3-2015", "9-2-2014")

# Then delete these known duplicates 
data1A_nodups <- data1A_distinct %>%
  filter(article_id_number %nin% dups)

# Now add back the correctly-coded versions of the duplicates 
master_dups <- rbind(data1A_nodups, dup_recoded) 

# Combine relevant Kidwell et al. (2016) data and the newly coded data to create Master 1A dataset----

# Merge master_dups dataset and relevant_kidwell_clean dataset to get master1Adataset 
master_1A_dataset <- merge(master_dups, relevant_kidwell_clean, by="article_id_number")

# Delete irrelevant/duplicated information between the Kidwell dataset and the 1A dataset
master_1A_dataset <- master_1A_dataset %>%
  select(-c(timestamp:journal)) %>%
  rename(journal = journal_code.x) # renaming 'journal_code.x" since the code above deletes the duplicate variable

# Checking the current study's judgements of empiricism to Kidwell et al.'s----------------

# Change no of experiments to numeric

master_1A_dataset$no_of_experiments <- as.character(master_1A_dataset$no_of_experiments)

# Create a new variable to make the new coding and Kidwell et al.'s coding of number of experiment in consistent format (char)
  # no_of_experiments is the newly coded variable, num_of_experiences is Kidwell et al.'s coding in character format
master_1A_dataset <- master_1A_dataset %>%
  mutate(num_of_experiments = case_when(number_of_experiments >= 5 ~ "5 or more", 
                                     number_of_experiments == 4 ~  "4", 
                                     number_of_experiments == 3 ~  "3",
                                     number_of_experiments == 2 ~  "2",
                                     number_of_experiments == 1 ~  "1",
                                     number_of_experiments == 0 ~  "0"))
                                 
# Mutate new variable that checks whether no (new coding) and num (Kidwell's coding) of experiments is the same                                       
master_1A_dataset <- master_1A_dataset %>%
  mutate(exp_check = case_when(no_of_experiments == num_of_experiments ~ "TRUE", 
                               no_of_experiments != num_of_experiments ~ "FALSE"))

# Filter only cases where it is NOT the same 
exp_check <- master_1A_dataset %>%
  filter(exp_check == FALSE) %>%
  relocate(num_of_experiments, .after = no_of_experiments) %>%
  mutate(kid_us_diff = case_when(num_of_experiments == 0 & no_of_experiments != 0 ~ "us_more", 
                                 num_of_experiments != 0 & no_of_experiments == 0 ~ "them_more")) %>%
  relocate(kid_us_diff, .after = num_of_experiments)

# Filtering only cases where we coded 0, Kidwell et al. coded >= 1, or vice versa
exp_check_followup <- exp_check %>%
  filter(kid_us_diff %in% c("us_more", "them_more"))

# Summary
  # There are 43 cases where our coding of the no. of experiments doesn't align with Kidwell's
  # In 4 of these cases, we coded more than 1 experiment, whilst Kidwell coded 0 
  # In 1 of these cases, we coded 0, whilst Kidwell coded more than 1
  # The senior coder checked these 5 cases, and in all the cases our judgement of empiricism is correct, NOT Kidwell's
  # So going forward, we used our no_of_experiments NOT num_of_experiments

# Since we're using our judgement of empiricism, we can delete Kidwell's coding of num_of_experiments
# We can also delete number_of_experiments and exp_check as these variables are no longer required
# Let's use the select function to keep the variables we want and to remove those we don't
master_1A_dataset <- master_1A_dataset %>%
  select(article_id_number:type_of_software_other, did_the_article_receive_a_badge_for_open_data:corresponding_author_e_mail_address) %>%
  relocate(article_id_number, .after = article_id_number_and_title)

# Write and export Master csv-----

# Write master_1A_dataset_clean to csv

master_1A_dataset %>% write_csv(here::here("data_files", "master_dataset_1A.csv"))
