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

# First let's read in the master 1A dataset
master_dataset_1A <- read_csv(here("data_files", "master_dataset_1A.csv")) %>%
  clean_names()

# remove non-empirical articles 
master_dataset_1A_empirical <- master_dataset_1A %>%
  filter(no_of_experiments != "0")

# Let's assign each article to a subfield
subfield_test <- master_dataset_1A_empirical %>%
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

# Christina assessed all 36 NA cases, and decided upon a subfield for each case
psyc_subfield <- subfield_test %>%
  mutate(subfield = case_when("10-1-2015" == article_id_number ~ "Cognition", 
                              "10-3-2014" == article_id_number ~ "Perception",
                              "10-4-2015" == article_id_number ~ "Perception",
                              "10-5-2015" == article_id_number ~ "Perception",
                              "11-5-2015" == article_id_number ~ "Cognition",
                              "12-10-2014" == article_id_number ~ "Social Psychology",
                              "12-4-2014" == article_id_number ~ "Social Psychology",
                              "12-7-2014" == article_id_number ~ "Cognition", 
                              "13-10-2014" == article_id_number ~ "Social Psychology",
                              "13-6-2014" == article_id_number ~ "Cognition",
                              "15-6-2014" == article_id_number ~ "Cognition",
                              "16-12-2014" == article_id_number ~ "Cognition",
                              "16-2-2015" == article_id_number ~ "Social Psychology",
                              "16-6-2014" == article_id_number ~ "Social Psychology",
                              "17-1-2014" == article_id_number ~ 'Cognition',
                              "17-2-2015" == article_id_number ~ "Social Psychology",
                              "19-4-2014" == article_id_number ~ "Cognition",
                              "19-4-2015" == article_id_number ~ "Social Psychology",
                              "2-4-2014" == article_id_number ~ "Cognition",
                              "2-5-2014" == article_id_number ~ "Perception",
                              "2-5-2015" == article_id_number ~ "Cognition",
                              "2-7-2014" == article_id_number ~ "Social Psychology",
                              "23-4-2014" == article_id_number ~ "Health Psychology",
                              "24-4-2014" == article_id_number ~ "Social Psychology",
                              "3-5-2014" == article_id_number ~ "Social Psychology",
                              "3-8-2014" == article_id_number ~ "Cognitive Neuroscience",
                              "34-2-2014" == article_id_number ~ "Social Psychology",
                              "4-1-2015" == article_id_number ~ "Social Psychology",
                              "4-12-2014" == article_id_number ~ "Social Psychology",
                              "4-7-2014" == article_id_number ~ "Cognition",
                              "5-2-2015" == article_id_number ~ "Health Psychology",
                              "5-8-2014" == article_id_number ~ "Perception",
                              "6-2-2015" == article_id_number ~ "Social Psychology",
                              "6-5-2015" == article_id_number ~ "Social Psychology",
                              "8-8-2014" == article_id_number ~ "Social Psychology",
                              TRUE ~ as.character(subfield)))

# After re-evaluation, we realised that the article "1-5-2015" was a non-empirical article, so we can remove it from the dataset
psyc_subfield_clean <- subset(psyc_subfield, article_id_number != "1-5-2015")

# let's summarise the articles by subfield

count_subfield <- psyc_subfield_clean %>%
  tabyl(subfield)

count_subfield %>%
  ggplot(aes(x = reorder(subfield, n), y = n)) +
  geom_col() +
  coord_flip()

subfield_summary <- psyc_subfield_clean %>%
  select(article_id_number, subfield)

# Now let's score the articles

# scoring
  # low items = 1
  # med items = 2
  # high items = 5

#First we need to make the data long
data_long <- psyc_subfield_clean %>%
  pivot_longer(everything(), names_to = "question", values_to = "response")

# Now let's assign scores for openness of data
data_scored_for_data <- data_long %>%
  mutate(data_score = case_when("software" == question & "No" == response ~ 0, "software" == question & "Yes" == response ~ 1,
                                "does_the_article_state_whether_or_not_the_data_are_available" == question & "No" == response ~ 0, "does_the_article_state_whether_or_not_the_data_are_available" == question & "Yes" == response ~ 1, 
                                "data_statement_indicates_that_data_are" == question & response %in% c("No statement", "Unavailable") ~ 0, "data_statement_indicates_that_data_are" == question & "Available" == response ~ 1,
                                
                                "how_are_data_accessible" == question & response %in% c("Email authors", "Not clear") ~ 0, "how_are_data_accessible" == question & response %in% c("Public dataset generated by the authors", "Public dataset generated by others", "PS Supplemental Materials", "Other", "Both public dataset generated by authors and public dataset generated by others") ~ 2, 
                                "does_the_data_url_go_to_a_working_page" == question & "No" == response ~ 0, "does_the_data_url_go_to_a_working_page" == question & "Yes" == response ~ 2,
                                "are_the_data_located_at_the_working_page" == question & response %in% c("Requires permission", "No") ~ 0, "are_the_data_located_at_the_working_page" == question & "Yes" == response ~ 2,
                                "can_the_data_be_downloaded" == question & response %in% c("Requires permission", "No") ~ 0, "can_the_data_be_downloaded" == question & "Yes" == response ~ 2,
                                "does_the_data_correspond_to_what_is_reported_in_the_article" == question & response %in% c("Unclear", "No") ~ 0, "does_the_data_correspond_to_what_is_reported_in_the_article" == question & "Yes" == response ~ 2,
                                "are_the_data_complete" == question & response %in% c("Unclear whether or not all of the data are available", "No, not all of the data are available") ~ 0, "are_the_data_complete" == question & "Yes, but only some of the data are available" == response ~ 1, "are_the_data_complete" == question & "Yes, all of the data appear to be available" == response ~ 2,
                                
                                "is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables" == question & "No" == response ~ 0, "is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables" == question & "Yes" == response ~ 5,
                                "are_analysis_scripts_included_with_the_data" == question & "No" == response ~ 0, "are_analysis_scripts_included_with_the_data" == question & "Yes" == response ~ 5)) %>%
  mutate(data_score = coalesce(data_score, 0))

# Let's create a single open data score for each article - CR struggling here

open_data_score_summary <- data_scored_for_data %>%
  group_by(article_id_number) %>% 
  summarise(totalscore = sum(data_score))

# And let's assign scores for openness of materials 
data_scored_for_materials <- data_long %>%
  mutate(materials_score = case_when("does_the_article_state_whether_or_not_any_research_materials_are_available" == question & "No" == response ~ 0, "does_the_article_state_whether_or_not_any_research_materials_are_available" == question & "Yes" == response ~ 1, 
                                     "statement_indicates_that_materials_are" == question & response %in% c("No statement", "Unavailable") ~ 0, "statement_indicates_that_materials_are" == question & "Available" == response ~ 1,
                                     
                                     "how_are_materials_accessible_please_only_fill_out_this_question_and_the_questions_below_if_the_articles_statement_indicates_that_the_materials_are_available" == question & response %in% c("Email authors", "Not clear") ~ 0, "how_are_materials_accessible_please_only_fill_out_this_question_and_the_questions_below_if_the_articles_statement_indicates_that_the_materials_are_available" == question & response %in% c("Public set of materials generated by the authors", "Public set of materials generated by others", "Both public set of materials generated by authors and generated by others", "PS Supplemental Materials", "Other") ~ 2, 
                                     "does_the_materials_url_go_to_a_working_page" == question & "No" == response ~ 0, "does_the_materials_url_go_to_a_working_page" == question & "Yes" == response ~ 2,
                                     "are_the_materials_located_at_the_working_page" == question & response %in% c("Requires permission", "No") ~ 0, "are_the_materials_located_at_the_working_page" == question & "Yes" == response ~ 2,
                                     "can_the_materials_be_downloaded" == question & response %in% c("Requires permission", "No") ~ 0, "can_the_materials_be_downloaded" == question & "Yes" == response ~ 2,
                                     "do_the_materials_correspond_to_what_is_reported_in_the_article" == question & response %in% c("Unclear", "No") ~ 0, "do_the_materials_correspond_to_what_is_reported_in_the_article" == question & "Yes" == response ~ 2,
                                     "are_the_materials_complete" == question & response %in% c("Unclear whether or not all of the materials are available", "No, not all of the materials are available") ~ 0, "are_the_materials_complete" == question & "Yes, but only some of the materials are available" == response ~ 1, "are_the_materials_complete" == question & "Yes, all of the materials appear to be available" == response ~ 2,
                                     "does_the_article_state_whether_there_is_supplemental_information_available" == question & response %in% c("Yes, but it is not freely accessible", "No") ~ 0, "does_the_article_state_whether_there_is_supplemental_information_available" == question & "Yes, and it is freely accessible" == response ~ 2,
                                     
                                     "are_analysis_scripts_included_with_the_data" == question & "No" == response ~ 0, "are_analysis_scripts_included_with_the_data" == question & "Yes" == response ~ 5)) %>%
  mutate(materials_score = coalesce(materials_score, 0))

# Let's create a single open materials score for each article - CR struggling here

open_materials_score_summary <- data_scored_for_materials %>%
  group_by(article_id_number) %>% 
  summarise(totalscore = sum(materials_score))

# Now let's create a new dataframe that combines subfield, open data score and open material score based on article ID

overall_summary <- full.join(subfield_summary, open_data_score_summary, open_materials_score_summary, by="article_id_number")

---------
# CR I created this code at the beginning but I don't think we'll need it

# Create separate dataframes for each subfield 

# behavioural neuroscience
behavioural_neuroscience <- psyc_subfield %>%
  filter(subfield == "Behavioural_Neuroscience")

# developmental psychology
developmental_psychology <- psyc_subfield %>%
  filter(subfield == "Developmental_Psychology")

# cognitive neuroscience 
cognitive_neuroscience <- psyc_subfield %>%
  filter(subfield == "Cognitive_Neuroscience")

# perception
perception <- psyc_subfield %>%
  filter(subfield == "Perception")

# social psychology 
social_psychology <- psyc_subfield %>%
  filter(subfield == "Social_Psychology")

# cognition
cognition <- psyc_subfield %>%
  filter(subfield == "Cognition")

# health psychology 
health_psychology <- psyc_subfield %>%
  filter(subfield == "Health_Psychology")



