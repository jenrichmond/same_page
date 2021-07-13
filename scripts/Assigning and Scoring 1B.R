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

# remove non-empirical articles 
data1B_empirical <- data1B %>%
  filter(no_of_experiments != "0")

# Assigning articles to a subfield----

# Let's assign each article to a subfield

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

# Now let's score the articles

# scoring
# low items = 1
# med items = 2
# high items = 5

glimpse(psyc_subfield)

#First we need to change data_page_no to a character variable (from a numeric variable)
psyc_subfield <- transform(psyc_subfield, "data_page_no" = as.character(data_page_no))

# Next we need to make the data long
data_long <- psyc_subfield %>%
  pivot_longer(everything(), names_to = "question", values_to = "response")

# Now let's assign scores for openness of data
data_scored_for_data <- data_long %>%
  mutate(data_score = case_when("software" == "question" & "No" == response ~ 0, "software" == "question" & "Yes" == response ~ 1,
                                "data_statement_present" == "question" & "No" == response ~ 0, "data_statement_present" == "question" & "Yes" == response ~ 1,
                                "data_statement_indicates" == "question" & "Unavailable" == response ~ 0, "data_statement_indicates" == "question" & "Available" == response  ~ 1,

                                "data_accessible" == "question" & "Not clear" == response ~ 0, "data_accessible" == "question" & response %in% c("Public dataset generated by the authors", "Public dataset generated by others", "Other (please specify):") ~ 2, 
                                "dataset_URL_working" == "question" & "No" == response ~ 0, "dataset_URL_working" == "question" & "Yes" == response ~ 2,
                                "data_locatable" == "question" & response %in% c("Requires permission", "No") ~ 0, "data_locatable" == "question" & "Yes" == response ~ 2,
                                "data_downloadable" == "question" & response %in% c("Requires permission", "No") ~ 0, "data_downloadable" == "question" & "Yes" == response ~ 2,
                                "data_correspond" == "question" & response %in% c("Unclear", "No") ~ 0, "data_correspond" == "question" & "Yes" == response ~ 2,
                                "data_complete" == "question" & response %in% c("Unclear whether or not all the data are available", "No, not all of the data are available") ~ 0, "data_complete" == "question" & "Yes, but only some of the data are available" == response ~ 1, "data_complete" == "question" & "Yes, all of the data appear to be available" == response ~ 2,
                                
                                "data_codebook" == "question" & "No" == response ~ 0, "data_codebook" == "question" & "Yes" == response ~ 5,
                                "data_scripts" == "question" & "No" == response ~ 0, "data_scripts" == "question" & "Yes" == response ~ 5)) %>%
  mutate(data_score = coalesce(data_score, 0))

# Let's create a single open data score for each article - CR struggling here

open_data_score_summary <- data_scored_for_data %>%
  group_by("question" == "article_id_number") %>% 
  summarise(totalscore = sum(data_score))

open_data_score_summary <- data_scored_for_data %>%
  filter("question" == "article_id_number") %>% 
  mutate(totalscore = sum(data_score))


materials_scoring <- psyc_subfield %>%
  select(article_id_number, materials_statement_present, materials_statement_indicates, materials_accessible, materials_URL_working, materials_locatable, materials_downloadable, materials_correspond, materials_complete, materials_explanation) %>%
  pivot_longer(names_to = "question", values_to = "response", materials_statement_present:materials_explanation)
  
  
  
  # helen issue come back to this
  
  filter(article_id_number == "2019-30-8-1123")


# And let's assign scores for openness of materials 
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


# this code edits the materials accessible score from 2 to 1 for articleID 2019-30-8-1123, this case was requiring permission

data_scored_for_materials <- data_scored_for_materials %>%
  mutate(materials_score = case_when(article_id_number == "2019-30-8-1123" & question == "materials_accessible" & response == "Other (please specify):" ~ 1, 
                                     TRUE ~ as.numeric(materials_score)))


# There is one case which I'm not sure how to code for
  # 2019-30-8-1123 Helen coded "Other (please specify)" for materials_accessible, but then went on to code "Upon Request" for materials_accessible_other --> this article should receive 0
# would it work if I entered this code: "materials_accessible_other" == question & "Upon Request" == response ~ -2
# would that cancel it out?


# Let's create a single open materials score for each article - struggling here

open_materials_score_summary <- data_scored_for_materials %>%
  group_by(article_id_number) %>% 
  summarise(totalscore = sum(materials_score))


open_materials_score_summary %>%
  ggplot(aes(x = totalscore)) +
  geom_histogram() +
  facet_wrap(~ badge)

summary(open_materials_score_summary)

open_materials_score_summary %>%
  tabyl(totalscore)

# Now let's create a new dataframe that combines subfield, open data score and open material score based on article ID

overall_summary <- full.join(subfield_summary, open_data_score_summary, open_materials_score_summary, by="article_id_number")
