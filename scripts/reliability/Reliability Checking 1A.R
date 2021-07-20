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

# Read in Reliability Checking data

data <- read_csv(here("data_files", "data1A_reliability_checking")) %>%
  select(coder_name, article_id_number_and_title, id_number = journal_code, participants:topic_other)

# Let's assign the articles to a subfield

psyc_subfield <- data %>%
  mutate(subfield = case_when("Animals" == participants ~ "Behavioural Neuroscience", 
                              "Humans" == participants & "0-18 years or 65 years+" == age ~ "Developmental Psychology", 
                              "Humans" == participants & "Brain" == brain_beh  ~ "Cognitive Neuroscience",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Sensation" == topic ~ "Perception",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Emotion, personality, social behaviour" == topic ~ "Social Psychology",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Intelligence, memory, decision making, reasoning, language, problem solving, creative thinking" == topic ~ "Cognition",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Fitness, weight, consumption, hormone levels, chemical uptake, sleeping patterns" == topic ~ "Health Psychology")) %>%
  relocate(subfield, .after = id_number)

psyc_subfield <- psyc_subfield %>%
  distinct()  # removing dup 1 x Jenn 1A5 coded twice


subfield_long <- psyc_subfield %>%
  select(coder_name, id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) 

psyc_subfield %>%
  get_dupes(coder_name, id_number) 

jenn <- psyc_subfield %>%
  filter(coder_name == "Jenn Lee" & id_number == "1A5") 

# CR realised that coders weren't required to code for data and materials --> we can't use this information to measure reliability
# perhaps we can use their subfield coding to assess reliability?
