# Load packages
library(tidyverse)
library(janitor)
library(here)
library(Hmisc)

# Read and clean dataset -----

data1B <- read_csv(here("OSF", "Data Files", "Study 1B Master Dataset.csv"))

# Remove non-empirical articles 
data1B_empirical <- data1B %>%
  filter(no_of_experiments != "0")

# SUBFIELD ASSIGNMENT -------

# Assign each article to a subfield

subfield_test <- data1B_empirical %>%
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

# The senior coder assessed all 17 NA cases, and decided upon a subfield for each case
psyc_subfield <- subfield_test %>%
  mutate(subfield = case_when("2020-31-1-65" == article_id_number ~ "Cognition",
                              "2019-30-8-1123" == article_id_number ~ "Cognition",
                              "2019-30-9-1259" == article_id_number ~ "Cognition",
                              "2019-30-8-1174" == article_id_number ~ "Cognition",
                              "2019-30-8-1186" == article_id_number ~ "Social Psychology", 
                              "2019-30-8-1195" == article_id_number ~ "Cognition",
                              "2020-31-11-1386" == article_id_number ~ "Cognition",
                              "2020-31-5-582" == article_id_number ~ "Social Psychology",
                              "2020-31-6-623" == article_id_number ~ "Social Psychology",
                              "2020-31-6-729" == article_id_number ~ "Behavioural Neuroscience",
                              "2020-31-7-781" == article_id_number ~ "Developmental Psychology",
                              "2020-31-7-865" == article_id_number ~ "Social Psychology",
                              "2020-31-5-505" == article_id_number ~ "Perception",
                              "2020-31-4-449" == article_id_number ~ "Health Psychology",
                              "2020-31-7-873" == article_id_number ~ "Developmental Psychology",
                              "2020-31-10-1245" == article_id_number ~ "Cognition",
                              "2020-31-10-1222" == article_id_number ~ "Social Psychology", 
                              TRUE ~ as.character(subfield))) # this line of code keeps the existing subfield values (that didn't need to be assigned manually), rather than replacing them with NAs  

# OPEN DATA SCORES -------

# Change data_page_no to a character variable (from a numeric variable)
psyc_subfield <- transform(psyc_subfield, "data_page_no" = as.character(data_page_no))

# Make the relevant data long
data_scoring <- psyc_subfield %>%
  select(article_id_number, subfield, software, data_statement_present, data_statement_indicates, data_accessible, dataset_URL_working, data_locatable, data_downloadable, data_correspond, data_complete, data_codebook, data_scripts) %>%
  pivot_longer(names_to = "question", values_to = "response", software:data_scripts)

# Assign scores to data sharing variables 
data_scored_for_data <- data_scoring %>%
  mutate(data_score = case_when(question == "software" & response == "No" ~ 0, 
                                question == "software" & response == "Yes" ~ 1, 
                                question == "data_statement_present" & response == "No" ~ 0, 
                                question == "data_statement_present" & response == "Yes" ~ 1,
                                question == "data_statement_indicates" & response == "Unavailable" ~ 0, 
                                question == "data_statement_indicates" & response == "Available" ~ 1,

                                question == "data_accessible" & response == "Not clear" ~ 0, 
                                question == "data_accessible" & response %in% c("Public dataset generated by the authors", "Public dataset generated by others", "Other (please specify):") ~ 2, 
                                question == "dataset_URL_working" & response == "No" ~ 0, 
                                question == "dataset_URL_working" & response == "Yes" ~ 2,
                                question == "data_locatable" & response == "No" ~ 0,
                                question == "data_locatable" & response == "Requires permission" ~ 1, 
                                question == "data_locatable" & response == "Yes" ~ 2,
                                question == "data_downloadable" & response == "No" ~ 0,
                                question == "data_downloadable" & response == "Requires permission" ~ 1, 
                                question == "data_downloadable" & response == "Yes" ~ 2,
                                question == "data_correspond" & response %in% c("Unclear", "No") ~ 0, 
                                question == "data_correspond" & response == "Yes" ~ 2,
                                question == "data_complete" & response %in% c("Unclear whether or not all the data are available", "No, not all of the data are available") ~ 0, 
                                question == "data_complete" & response == "Yes, but only some of the data are available" ~ 1, 
                                question == "data_complete" & response == "Yes, all of the data appear to be available" ~ 2,
                                
                                question == "data_codebook" & response == "No" ~ 0, 
                                question == "data_codebook" & response == "Yes" ~ 5,
                                question == "data_scripts" & response == "No" ~ 0, 
                                question == "data_scripts" & response == "Yes" ~ 5)) %>%
  mutate(data_score = coalesce(data_score, 0))

# Calculate the Open Data Score for each article

open_data_score_summary <- data_scored_for_data %>%
  group_by(article_id_number, subfield) %>% 
  summarise(open_data_score = sum(data_score))

# OPEN MATERIALS SCORE -----

# Make the relevant data long
materials_scoring <- psyc_subfield %>%
  select(article_id_number, subfield, materials_statement_present, materials_statement_indicates, materials_accessible, materials_URL_working, materials_locatable, materials_downloadable, materials_correspond, materials_complete, materials_explanation) %>%
  pivot_longer(names_to = "question", values_to = "response", materials_statement_present:materials_explanation)

# Assign scores to materials sharing variables 
data_scored_for_materials <- materials_scoring %>%
  mutate(materials_score = case_when(question == "materials_statement_present" & response =="No" ~ 0,
                                     question == "materials_statement_present"  &  response == "Yes" ~ 1, 
                                     question == "materials_statement_indicates" & response =="Unavailable" ~ 0,
                                     question == "materials_statement_indicates"  &  response == "Available" ~ 1, 
                                     
                                     question == "materials_accessible" & response =="Not clean" ~ 0,
                                     question == "materials_accessible"  &response %in% c("Public set of materials generated by authors", "Public set of materials generated by others", "Other (please specify):") ~ 2, 
                                     question == "materials_URL_working" & response =="No" ~ 0,
                                     question == "materials_URL_working"  &  response == "Yes" ~ 2, 
                                     question == "materials_locatable"  & response== "No" ~ 0,
                                     question == "materials_locatable" & response == "Requires permission" ~ 1,
                                     question == "materials_locatable" & response == "Yes" ~ 2,
                                     question == "materials_downloadable"  & response== "No" ~ 0,
                                     question == "materials_downloadable" & response == "Requires permission" ~ 1,
                                     question == "materials_downloadable" & response == "Yes" ~ 2,
                                     question == "materials_correspond" & response %in% c("Unclear", "No") ~ 0,
                                     question == "materials_correspond" & response == "Yes" ~ 2,
                                     question == "materials_complete" & response %in% c("Unclear whether or not all the materials are available", "No, not all of the materials are available") ~ 0,
                                     question == "materials_complete" & response == "Yes, but only some of the materials are available" ~ 1,
                                     question == "materials_complete" & response == "Yes, all of the materials appear to be available" ~ 2,

                                     question == "materials_explanation" & response == "No" ~ 0,
                                     question == "materials_explanation" & response == "Yes" ~ 5))  %>%
  mutate(materials_score = coalesce(materials_score, 0))

# This code edits the materials accessible score from 2 to 1 for articleID 2019-30-8-1123, as this case was requiring permission to access the materials
data_scored_for_materials <- data_scored_for_materials %>%
  mutate(materials_score = case_when(article_id_number == "2019-30-8-1123" & question == "materials_accessible" & response == "Other (please specify):" ~ 1, 
                                     TRUE ~ as.numeric(materials_score)))

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

total_summary <- left_join(psyc_subfield, scores, by = "article_id_number") %>%
  select(article_id_number:open_materials_score)

# Export dataset as a csv. -----

total_summary %>% write_csv(here::here("data_files", "Scored Study 1B Master Dataset.csv"))
