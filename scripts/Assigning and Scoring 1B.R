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
  filter(!str_detect(q4_1,'Check'))

# Filter out unwanted duplicates----

# let's check whether there are any articles which have been coded more than once
duplicates <- get_dupes(data1B_sep, article_id_number) 

# to solve the duplicate problem, CR recoded all the duplicates (except for one, which she had coded originally), here filtering for her 4 recoded observations
dup_recoded <- duplicates %>%
  filter(coder_name == "Christina Rochios") %>%
  select(-dupe_count) 

# let's remove all duplicated rows from the clean data1A dataset using the distinct() function
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

# Removing non-empirical articles-----

# remove non-empirical articles 
data1B_empirical <- master_dups %>%
  filter(no_of_experiments != "0")

glimpse(data1B_empirical)

# Assigning articles to a subfield----

install.packages("tibble")
library(tibble)

# Let's assign each article to a subfield
(psyc_subfield) <- data1B_empirical %>%
  mutate(subfield = case_when("Animals" == participants ~ "Behavioural Neuroscience", 
                              "Humans" == participants & "0-18 years or 65 years+" == age ~ "Developmental Psychology", 
                              "Humans" == participants & "Brain" == brain_beh  ~ "Cognitive Neuroscience",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Sensation" == topic ~ "Perception",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Emotion, personality, social behaviour" == topic ~ "Social Psychology",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Intelligence, memory, decision making, reasoning, language, problem solving, creative thinking" == topic ~ "Cognition",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Fitness, weight, consumption, hormone levels, chemical uptake, sleeping patterns" == topic ~ "Health Psychology")) %>%
  relocate(psyc_subfield, .before = journal.x)

# Christina stuck here - I'm getting an error message that says "Error: Must subset columns with a valid subscript vector.x Subscript has the wrong type `tbl_df<"

# Some articles wouldn't have been assigned to a subfield with the previous function, as they would have fallen into the 'other' category 
# let's assign these 'other' articles to a subfield manually
psyc_subfield %>%
  select(subfield != "Behavioural_Neuroscience", "Developmental_Psychology", "Cognitive_Neuroscience", "Perception", "Social_Psychology", "Cognition", "Health_Psychology")
# finish this code once you know which articles need to be assigned manually 

#Check NA values

na_check <- psyc_subfield %>%
  filter(is.na(subfield))

# let's summarise the articles by subfield

subfield_summary <- psyc_subfield %>%
  select(article_id_number, subfield)

# Now let's score the articles

# scoring
# low items = 1
# med items = 2
# high items = 5

#First we need to make the data long
data_long <- psyc_subfield %>%
  pivot_longer(names_to = "question", values_to = "response", data_badge:URL_supplemental_info)

# Now let's assign scores for openness of data
data_scored_for_data <- data_long %>%
  mutate(data_score = case_when("software" == question & "No" == response ~ 0, "software" == question & "Yes" == response ~ 1,
                                "data_statement_present" == question & "No" == response ~ 0, question == "data_statement_present" == question & "Yes" == response ~ 1, 
                                "data_statement_indicates" == question & response %in% c("No_statement", "Unavailable") ~ 0, "data_statement_indicates" == question & "Available" == response  ~ 1,

                                "data_accessible" == question & "Not_clear" == response ~ 0, "data_accessible" == question & response %in% c("Public_dataset_generated_by_authors", "Public_dataset_generated_by_others", "Other") ~ 2, 
                                "dataset_URL_working" == question & "No" == response ~ 0, question == "dataset_URL_working" == question & "Yes" == response ~ 2,
                                "data_locatable" == question & response %in% c("Requires_permission", "No") ~ 0, "data_locatable" == question & "Yes" == response ~ 2,
                                "data_downloadable" == question & response %in% c("Requires_permission", "No") ~ 0, "data_downloadable" == question & "Yes" == response ~ 2,
                                "data_correspond" == question & response %in% c("Unclear", "No") ~ 0, "data_correspond" == question & "Yes" == response ~ 2,
                                "data_complete" == question & response %in% c("Unclear_whether_or_not_all_of_the_data_are_available", "No,_not_all_of_the_data_are_available") ~ 0, "data_complete" == question & "Yes,_but_only_some_of_the_data_are_available" == response ~ 1, "data_complete" == question & "Yes,_all_of_the_data_appear_to_be_available" == response ~ 2,
                                
                                "data_codebook" == question & "No" == response ~ 0, "data_codebook" == question & "Yes" == response ~ 5,
                                "data_scripts" == question & "No" == response ~ 0, "data_scripts" == question & "Yes" == response ~ 5))

# Christina trying another way - doesn't look like it's working 
data_scored <- data_long %>%
  mutate(data_score = case_when("No" == "software" ~ 0, "Yes" == "software" ~ 1, 
                                "No" == "data_statement_present" ~ 0, "Yes" == "data_statement_present" ~ 1,
                                "data_statement_indicates" %in% c("No_statement", "Unavailable") ~ 0, "Available" = "data_statement_indicates" ~ 1))

# Let's create a single open data score for each article

open_data_score_summary <- data_scored_for_data %>%
  group_by(article_id_number) %>% 
  summarise(totalscore = sum(data_score))

# And let's assign scores for openness of materials 
data_scored_for_materials < data_long %>%
  mutate(materials_score = case_when(question == "materials_statement_present" & response == "No" ~ 0, question == "materials_statement_present" & response == "Yes" ~ 1, 
                                     question == "materials_statement_indicates" & response %in% c("No_statement", "Unavailable") ~ 0, question == "materials_statement_indicates" & response == "Available" ~ 1,
                                     
                                     question == "materials_accessible" & response == "Not_clear" ~ 0, question == "materials_accessible" & response %in% ("Public_set_generated_by_authors", "Public_set_generated_by_others", "Other") ~ 2, 
                                     question == "materials_URL_working" & response == "No" ~ 0, question == "materials_URL_working" & response == "Yes" ~ 2,
                                     question == "materials_locatable" & response %in% c("Requires_permission", "No") ~ 0, question == "materials_locatable" & response == "Yes" ~ 2,
                                     question == "materials_downloadable" & response %in% c("Requires_permission", "No") ~ 0, question == "materials_downloadable" & response == "Yes" ~ 2,
                                     question == "materials_correspond" & response %in% c("Unclear", "No") ~ 0, question == "materials_correspond" & response == "Yes" ~ 2,
                                     question == "materials_complete" & response %in% c("Unclear_whether_or_not_all_of_the_materials_are_available", "No,_not_all_of_the_materials_are_available") ~ 0, question == "materials_complete" & response == "Yes,_but_only_some_of_the_materials_are_available" ~ 1, question == "materials_complete" & response == "Yes,_all_of_the_materials_appear_to_be_available" ~ 2,
                                     question == "supplemental_info" & response %in% c("Yes,_but_it_is_not_freely_accessible", "No") ~ 0,question == "supplemental_info" & response == "Yes,_and_it_is_freely_accessible" ~ 2
                                     
                                     question == "materials_explanation" & response == "No" ~ 0, question == "materials_explanation" & response == "Yes" ~ 5,))

# Let's create a single open materials score for each article

open_materials_score_summary <- data_scored_for_materials %>%
  group_by(article_id_number) %>% 
  summarise(totalscore = sum(materials_score))

# Now let's create a new dataframe that combines subfield, open data score and open material score based on article ID

overall_summary <- full.join(subfield_summary, open_data_score_summary, open_materials_score_summary, by="article_id_number")
