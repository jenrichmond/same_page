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

# Read and clean data -----

# read in 1B data from csv

data1B <- read_csv(here("data_files", "master_dataset_1B.csv"))

# Assigning articles to a subfield----

# Let's assign each article to a subfield

subfield_test <- data1B %>%
  mutate(subfield = case_when("Animals" == participants ~ "Behavioural Neuroscience", 
                              "Humans" == participants & "0-18 years or 65 years+" == age ~ "Developmental Psychology", 
                              "Humans" == participants & "Brain" == brain_beh  ~ "Cognitive Neuroscience",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Sensation" == topic ~ "Perception",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Emotion, personality, social behaviour" == topic ~ "Social Psychology",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Intelligence, memory, decision making, reasoning, language, problem solving, creative thinking" == topic ~ "Cognition",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Fitness, weight, consumption, hormone levels, chemical uptake, sleeping patterns" == topic ~ "Health Psychology")) %>%
  relocate(subfield, .after = no_of_experiments)

# Some articles wouldn't have been assigned to a subfield with the previous function, as they would have fallen into the 'other' category 
# let's assign these 'other' articles to a subfield manually
na_articles <- subfield_test %>%
  filter(is.na(subfield)) %>%
  select(coder_name:subfield, topic_other)

# Christina assessed all 19 NA cases, and decided upon a subfield for each case
psyc_subfield <- subfield_test %>%
  mutate(subfield = case_when("2020-31-1-65" == article_id_number ~ "Cognition",
                              "2019-30-8-1123" == article_id_number ~ "Cognition",
                              "2019-30-9-1259" == article_id_number ~ "Cognition",
                              "2019-30-8-1174" == article_id_number ~ "Cognition",
                              "2019-30-8-1186" == article_id_number ~ "Social Psychology", 
                              "2019-30-8-1195" == article_id_number ~ "Cognition",
                              "2020-31-11-1386" == article_id_number ~ "Cognition",
                              "2019-30-9-1362" == article_id_number ~ "Cognition",
                              "2020-31-5-582" == article_id_number ~ "Social Psychology",
                              "2020-31-6-623" == article_id_number ~ "Social Psychology",
                              "2020-31-6-729" == article_id_number ~ "Behavioural Neuroscience",
                              "2020-31-7-781" == article_id_number ~ "Developmental Psychology",
                              "2020-31-7-865" == article_id_number ~ "Social Psychology",
                              "2020-31-5-505" == article_id_number ~ "Perception",
                              "2020-31-4-449" == article_id_number ~ "Health Psychology",
                              "2020-31-7-873" == article_id_number ~ "Developmental Psychology",
                              "2020-31-8-1013" == article_id_number ~ "Social Psychology",
                              "2020-31-10-1245" == article_id_number ~ "Cognition",
                              "2020-31-10-1222" == article_id_number ~ "Social Psychology", 
                              TRUE ~ as.character(subfield))) # this line of code keeps the existing subfield values (that didn't need to be assigned manually), rather than replacing them with NAs  

# let's check that all articles have been assigned to a subfield
na_articles_check <- psyc_subfield %>%
  filter(is.na(subfield))
  
# let's summarise the articles by subfield

count_subfield <- psyc_subfield %>%
  tabyl(subfield)

count_subfield %>%
  ggplot(aes(x = reorder(subfield, n), y = n)) +
  geom_col() +
  coord_flip()

subfield_summary <- psyc_subfield %>%
  select(article_id_number, subfield)

# CHRISTINA UP TO HERE

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
