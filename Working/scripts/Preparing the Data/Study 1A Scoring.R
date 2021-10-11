# Load packages
library(tidyverse)
library(janitor)
library(here)
library(Hmisc)

# Read and clean dataset -----

master_dataset_1A <- read_csv(here("Working", "data_files", "Study 1A Master Dataset.csv")) 
  # Note: you will need to change the location of the file, depending on where you have saved it

# Remove non-empirical articles 
master_dataset_1A_empirical <- master_dataset_1A %>%
  filter(no_of_experiments != "0")


# SUBFIELD ASSIGNMENT ------------

# Assign each article to a subfield
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
# The senior coder assigned these 'other' articles to a subfield manually
na_articles <- subfield_test %>%
  filter(is.na(subfield)) %>%
  select(article_id_number:subfield, topic_other)

# The senior coded assessed all 36 NA cases, and decided upon a subfield for each case
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

# OPEN DATA SCORES  ------------

# Make the relevant data long

data_scoring <- psyc_subfield_clean %>%
  select(article_id_number, subfield, software, does_the_article_state_whether_or_not_the_data_are_available, data_statement_indicates_that_data_are, how_are_data_accessible, does_the_data_url_go_to_a_working_page, are_the_data_located_at_the_working_page, can_the_data_be_downloaded, does_the_data_correspond_to_what_is_reported_in_the_article, are_the_data_complete, is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables, are_analysis_scripts_included_with_the_data) %>%
  pivot_longer(names_to = "question", values_to = "response", software:are_analysis_scripts_included_with_the_data)


# Assign scores to data sharing variables 

data_scored_for_data <- data_scoring %>%
  mutate(data_score = case_when(question == "software" & response == "No" ~ 0, 
                                question == "software" & response == "Yes" ~ 1,
                                question == "does_the_article_state_whether_or_not_the_data_are_available" & response == "No" ~ 0, 
                                question == "does_the_article_state_whether_or_not_the_data_are_available" & response == "Yes" ~ 1, 
                                question == "data_statement_indicates_that_data_are" & response %in% c("No statement", "Unavailable") ~ 0, 
                                question == "data_statement_indicates_that_data_are" & response == "Available" ~ 1,
                                question == "how_are_data_accessible" & response %in% c("Email authors", "Not clear") ~ 0, 
                                question == "how_are_data_accessible" & response %in% c("Public dataset generated by the authors", "Public dataset generated by others", "PS Supplemental Materials", "Other", "Both public dataset generated by authors and public dataset generated by others") ~ 2, 
                                question == "does_the_data_url_go_to_a_working_page" & response == "No" ~ 0, 
                                question == "does_the_data_url_go_to_a_working_page" & response == "Yes" ~ 2,
                                question == "are_the_data_located_at_the_working_page" & response == "No" ~ 0, 
                                question == "are_the_data_located_at_the_working_page" & response == "Requires permission" ~ 1, 
                                question == "are_the_data_located_at_the_working_page" & response == "Yes" ~ 2,
                                question == "can_the_data_be_downloaded" & response == "No" ~ 0,
                                question == "can_the_data_be_downloaded" & response == "Requires permission" ~ 1, 
                                question == "can_the_data_be_downloaded" & response == "Yes" ~ 2,
                                question == "does_the_data_correspond_to_what_is_reported_in_the_article" & response %in% c("Unclear", "No") ~ 0, 
                                question == "does_the_data_correspond_to_what_is_reported_in_the_article" & response == "Yes" ~ 2,
                                question == "are_the_data_complete" & response %in% c("Unclear whether or not all of the data are available", "No, not all of the data are available") ~ 0, 
                                question == "are_the_data_complete" & response == "Yes, but only some of the data are available" ~ 1, 
                                question == "are_the_data_complete" & response == "Yes, all of the data appear to be available" ~ 2,
                                
                                question == "is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables" & response == "No" ~ 0, 
                                question == "is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables" & response == "Yes" ~ 5,
                                question == "are_analysis_scripts_included_with_the_data" & response == "No" ~ 0, 
                                question == "are_analysis_scripts_included_with_the_data" & response == "Yes" ~ 5)) %>%
  mutate(data_score = coalesce(data_score, 0))

# Calculate the Open Data Score for each article 

open_data_score_summary <- data_scored_for_data %>%
  group_by(article_id_number, subfield) %>% 
  summarise(open_data_score = sum(data_score))

# OPEN MATERIALS SCORES ----------------

# Make the relevant data long 

materials_scoring <- psyc_subfield_clean %>%
  select(article_id_number, subfield, does_the_article_state_whether_or_not_any_research_materials_are_available, statement_indicates_that_materials_are, how_are_materials_accessible_please_only_fill_out_this_question_and_the_questions_below_if_the_articles_statement_indicates_that_the_materials_are_available, does_the_materials_url_go_to_a_working_page, are_the_materials_located_at_the_working_page, can_the_materials_be_downloaded, do_the_materials_correspond_to_what_is_reported_in_the_article, are_the_materials_complete, are_analysis_scripts_included_with_the_data) %>%
  pivot_longer(names_to = "question", values_to = "response", does_the_article_state_whether_or_not_any_research_materials_are_available:are_analysis_scripts_included_with_the_data)

# Assign scores to materials sharing variables 

data_scored_for_materials <- materials_scoring %>%
  mutate(materials_score = case_when(question == "does_the_article_state_whether_or_not_any_research_materials_are_available"& response == "No" ~ 0, 
                                     question == "does_the_article_state_whether_or_not_any_research_materials_are_available" & response == "Yes" ~ 1, 
                                     question == "statement_indicates_that_materials_are" & response %in% c("No statement", "Unavailable") ~ 0, 
                                     question == "statement_indicates_that_materials_are" & response == "Available" ~ 1,
                                     
                                     question == "how_are_materials_accessible_please_only_fill_out_this_question_and_the_questions_below_if_the_articles_statement_indicates_that_the_materials_are_available" & response %in% c("Email authors", "Not clear") ~ 0, 
                                     question == "how_are_materials_accessible_please_only_fill_out_this_question_and_the_questions_below_if_the_articles_statement_indicates_that_the_materials_are_available" & response %in% c("Public set of materials generated by the authors", "Public set of materials generated by others", "Both public set of materials generated by authors and generated by others", "PS Supplemental Materials", "Other") ~ 2, 
                                     question == "does_the_materials_url_go_to_a_working_page" & response == "No" ~ 0, 
                                     question == "does_the_materials_url_go_to_a_working_page" & response == "Yes" ~ 2,
                                     question == "are_the_materials_located_at_the_working_page" & response == "No" ~ 0,
                                     question == "are_the_materials_located_at_the_working_page" & response == "Requires permission" ~ 1,
                                     question == "are_the_materials_located_at_the_working_page" & response == "Yes" ~ 2,
                                     question == "can_the_materials_be_downloaded" & response == "No" ~ 0, 
                                     question == "can_the_materials_be_downloaded" & response == "Requires permission" ~ 1, 
                                     question == "can_the_materials_be_downloaded" & response == "Yes" ~ 2,
                                     question == "do_the_materials_correspond_to_what_is_reported_in_the_article" & response %in% c("Unclear", "No") ~ 0, 
                                     question == "do_the_materials_correspond_to_what_is_reported_in_the_article" & response == "Yes" ~ 2,
                                     question == "are_the_materials_complete" & response %in% c("Unclear whether or not all of the materials are available", "No, not all of the materials are available") ~ 0, 
                                     question == "are_the_materials_complete" & response == "Yes, but only some of the materials are available" ~ 1, 
                                     question == "are_the_materials_complete" & response == "Yes, all of the materials appear to be available" ~ 2,
                                     
                                     question == "are_analysis_scripts_included_with_the_data" & response == "No" ~ 0, 
                                     question == "are_analysis_scripts_included_with_the_data" & response == "Yes" ~ 5)) %>%
  mutate(materials_score = coalesce(materials_score, 0))

# Calculate the Open Materials Score for each article

open_materials_score_summary <- data_scored_for_materials %>%
  group_by(article_id_number, subfield) %>% 
  summarise(open_materials_score = sum(materials_score))

# Create a new dataframe that summarises each article's subfield-assignment, Open Data Score and Open Materials Score 

overall_summary <- left_join(open_materials_score_summary, open_data_score_summary, by ="article_id_number") 

overall_summary <- overall_summary %>%
  select(article_id_number, 
         subfield = subfield.x,
         open_data_score,
         open_materials_score)

# Create another dataframe that combines ALL data: collected data + subfield + data score + material score

scores <- overall_summary %>%
  select(article_id_number, open_data_score, open_materials_score)

total_summary <- left_join(psyc_subfield_clean, scores, by = "article_id_number")

# Export scored dataset as a csv. -----

total_summary %>% write_csv(here::here("data_files", "Scored Study 1A Master Dataset.csv"))




