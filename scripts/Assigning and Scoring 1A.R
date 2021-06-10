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
kidwell <- read_csv(here("data_files", "master_dataset_1A.csv")) %>%
  clean_names()

# remove non-empirical articles 
master_dataset_1A_empirical <- master_dataset_1A %>%
  filter(no_of_experiments != "0")
# CR before we do this, we might want to investigate the 1 case where we said the article was non-empirical but Kidwell said it was

# Let's assign each article to a subfield
psyc_subfield <- master_dataset_1A_empirical %>%
  mutate(subfield = case_when(participants == "Animals" ~ "Behavioural_Neuroscience", 
                              participants == "Humans" & age == "0-18 years or 65 years+" ~ "Developmental_Psychology", 
                              participants == "Humans" & brain_beh == "Brain"  ~ "Cognitive_Neuroscience",
                              participants == "Humans" & brain_beh == "Both"|"Behaviour" & topic == "Sensation" ~ "Perception", 
                              participants == "Humans" & brain_beh == "Both"|"Behaviour" & topic == "Emotion, personality, social behaviour" ~ "Social_Psychology",
                              participants == "Humans" & brain_beh == "Both"|"Behaviour" & topic == "Intelligence, memory, decision making, reasoning, language, problem solving, creative thinking" ~ "Cognition",
                              participants == "Humans" & brain_beh == "Both"|"Behaviour" & topic == "Fitness, weight, consumption, hormone levels, chemical uptake, sleeping patterns" ~ "Health_Psychology")) %>%
  relocate(psyc_subfield, .before = journal.x)


#Check NA values

na_check <- psyc_subfield %>%
  filter(is.na(subfield))

# let's summarise the articles by subfield

subfield_summary <- psyc_subfiled %>%
  select(article_id_number, subfield)

# Now let's score the articles

# scoring
  # low items = 1
  # med items = 2
  # high items = 5

#First we need to make the data long
data_long <- psyc_subfield %>%
  pivot_longer(names_to = "question", values_to = "response", software:if_materials_url_links_to_an_independent_archive_repository_which_repository)

# Now let's assign scores for openness of data
data_scored_for_data <- data_long %>%
  mutate(data_score = case_when(question == "software" & response == "No" ~ 0, question == "software" & response == "Yes" ~ 1,
                           question == "does_the_article_state_whether_or_not_the_data_are_available" & response == "No" ~ 0, question == "does_the_article_state_whether_or_not_the_data_are_available" & response == "Yes" ~ 1, 
                           question == "data_statement_indicates_that_data_are" & response %in% c("No_statement", "Unavailable") ~ 0, question == "data_statement_indicates_that_data_are" & response == "Available" ~ 1,
                           
                           question == "how_are_data_accessible" & response %in% c("Email_authors", "Not_clear") ~ 0, question == "how_are_data_accessible" & response %in% ("Public_dataset_generated_by_authors", "Public_dataset_generated_by_others", "PS_Supplemental_Materials", "Other") ~ 2, 
                           question == "does_the_data_url_go_to_a_working_page" & response == "No" ~ 0, question == "does_the_data_url_go_to_a_working_page" & response == "Yes" ~ 2,
                           question == "are_the_data_located_at_the_working_page" & response %in% c("Requires_permission", "No") ~ 0, question == "are_the_data_located_at_the_working_page" & response == "Yes" ~ 2,
                           question == "can_the_data_be_downloaded" & response %in% c("Requires_permission", "No") ~ 0, question == "can_the_data_be_downloaded" & response == "Yes" ~ 2,
                           question == "does_the_data_correspond_to_what_is_reported_in_the_article" & response %in% c("Unclear", "No") ~ 0, question == "does_the_data_correspond_to_what_is_reported_in_the_article" & response == "Yes" ~ 2,
                           question == "are_the_data_complete" & response %in% c("Unclear_whether_or_not_all_of_the_data_are_available", "No,_not_all_of_the_data_are_available") ~ 0, question == "are_the_data_complete" & response == "Yes,_but_only_some_of_the_data_are_available" ~ 1, question == "are_the_data_complete" & response == "Yes,_all_of_the_data_appear_to_be_available" ~ 2,
                           
                           question == "is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables" & response == "No" ~ 0, question == "is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables" & response == "Yes" ~ 5,
                           question == "are_analysis_scripts_included_with_the_data" & response == "No" ~ 0, question == "are_analysis_scripts_included_with_the_data" & response == "Yes" ~ 5))

# Let's create a single open data score for each article

open_data_score_summary <- data_scored_for_data %>%
  group_by(article_id_number) %>% 
  summarise(totalscore = sum(data_score))

# And let's assign scores for openness of materials 
data_scored_for_materials < data_long %>%
  mutate(materials_score = case_when(question == "does_the_article_state_whether_or_not_the_materials_are_available" & response == "No" ~ 0, question == "does_the_article_state_whether_or_not_the_materials_are_available" & response == "Yes" ~ 1, 
                                     question == "statement_indicates_that_materials_are" & response %in% c("No_statement", "Unavailable") ~ 0, question == "statement_indicates_that_materials_are" & response == "Available" ~ 1,
                                     
                                     question == "how_are_materials_accessible_please_only_fill_out_this_question_and_the_questions_below_if_the_articles_statement_indicates_that_materials_are_available" & response %in% c("Email_authors", "Not_clear") ~ 0, question == "how_are_materials_accessible_please_only_fill_out_this_question_and_the_questions_below_if_the_articles_statement_indicates_that_materials_are_available" & response %in% ("Public_set_generated_by_authors", "Public_set_generated_by_others", "PS_Supplemental_Materials", "Other") ~ 2, 
                                     question == "does_the_materials_url_go_to_a_working_page" & response == "No" ~ 0, question == "does_the_materials_url_go_to_a_working_page" & response == "Yes" ~ 2,
                                     question == "are_the_materials_located_at_the_working_page" & response %in% c("Requires_permission", "No") ~ 0, question == "are_the_materials_located_at_the_working_page" & response == "Yes" ~ 2,
                                     question == "can_the_materials_be_downloaded" & response %in% c("Requires_permission", "No") ~ 0, question == "can_the_materials_be_downloaded" & response == "Yes" ~ 2,
                                     question == "do_the_materials_correspond_to_what_is_reported_in_the_article" & response %in% c("Unclear", "No") ~ 0, question == "do_the_materials_correspond_to_what_is_reported_in_the_article" & response == "Yes" ~ 2,
                                     question == "are_the_materials_complete" & response %in% c("Unclear_whether_or_not_all_of_the_materials_are_available", "No,_not_all_of_the_materials_are_available") ~ 0, question == "are_the_materials_complete" & response == "Yes,_but_only_some_of_the_materials_are_available" ~ 1, question == "are_the_materials_complete" & response == "Yes,_all_of_the_materials_appear_to_be_available" ~ 2,
                                     question == "does_the_article_state_whether_supplemental_information_available" & response %in% c("Yes,_but_it_is_not_freely_accessible", "No") ~ 0,question == "does_the_article_state_whether_supplemental_information_available" & response == "Yes,_and_it_is_freely_accessible" ~ 2
                                     
                                     question == "are_analysis_scripts_included_with_the_data" & response == "No" ~ 0, question == "are_analysis_scripts_included_with_the_data" & response == "Yes" ~ 5,))

# Let's create a single open materials score for each article

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



