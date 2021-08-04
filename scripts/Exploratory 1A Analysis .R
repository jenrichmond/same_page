# Exploratory 1A Analysis 

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
library(ggeasy)
library(jmv)
library(psych)
library(afex)
library(rstatix)

# let's read in the data

data1A <- read_csv(here("data_files", "scored_master_dataset_1A.csv"))

# First factor: Subfield ------

# first, let's collapse the subfields into 4 main groups: developmental, cognition, social and 'other'
subfield_groups <- data1A %>%
  mutate(subfield_groups = case_when(subfield == "Behavioural Neuroscience" ~ "Other",
                                     subfield == "Cognitive Neuroscience" ~ "Other",
                                     subfield == "Health Psychology" ~ "Other",
                                     subfield == "Perception" ~ "Other",
                                     TRUE ~ as.character(subfield))) %>%
  relocate(subfield_groups, .after = subfield)

# we can now delete the original subfield column
subfield_groups <- subfield_groups %>%
  select(-subfield)

# Second factor: Time -----

# we want to group the data in 3 six months: first half of 2014, second half of 2014 and first half of 2015

dates <- subfield_groups %>%
  mutate(time_period = case_when(
    str_detect(article_id_number, "1-2014|2-2014|3-2014|4-2014|5-2014|6-2014") ~ "1st half 2014",
    str_detect(article_id_number, "7-2014|8-2014|9-2014|10-2014|11-2014|12-2014") ~ "2nd half 2014",
    str_detect(article_id_number, "1-2015|2-2015|3-2015|4-2015|5-2015") ~ "1st half 2015")) %>%
  relocate(time_period, .after = article_id_number)

dates$time_period <- fct_relevel(dates$time_period, c("1st half 2014", "2nd half 2014", "1st half 2015"))

levels(dates$time_period)

# Subfield vs. badges

# Data
subfield_databadges <- dates %>%
  tabyl(subfield_groups, did_the_article_receive_a_badge_for_open_data) %>%
  mutate("Percentage of articles that received an Open Data badge" = Yes/(Yes + No)*100)

# Materials
subfield_materialsbadges <- dates %>%
  tabyl(subfield_groups, did_the_article_receive_a_badge_for_open_materials.x) %>%
  mutate("Percentage of articles that received an Open Materials badge" = Yes/(Yes + No)*100)

# Time vs. badges 

# Data
time_databadges <- dates %>%
  tabyl(time_period, did_the_article_receive_a_badge_for_open_data) %>%
  mutate("Percentage of articles that received an Open Data badge" = Yes/(Yes + No)*100)

# Materials
time_materialsbadges <- dates %>%
  tabyl(time_period, did_the_article_receive_a_badge_for_open_materials.x) %>%
  mutate("Percentage of articles that received an Open Materials badge" = Yes/(Yes + No)*100)

# Subfield vs. codebook/scripts/understanding variables

# Data
subfield_codebook <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables) %>%
  mutate("Percentage of articles with reportedly available data that included a codebook" = Yes/(Yes + No)*100) 

subfield_scripts <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(subfield_groups, are_analysis_scripts_included_with_the_data) %>%
  mutate("Percentage of articles with reportedly available data that included analysis scripts" = Yes/(Yes + No)*100)

# Materials
subfield_materials_usable <- dates %>%
  filter(statement_indicates_that_materials_are == "Available") %>%
  tabyl(subfield_groups, are_analysis_scripts_included_with_the_materials) %>%
  mutate("Percentage of articles with reportedly available materials that included material scripts" = Yes/(Yes + No)*100)

# Time vs. codebook/scripts/understanding variables

# Data
time_codebook <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(time_period, is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables) %>%
  mutate("Percentage of articles with reportedly available data that included a codebook" = Yes/(Yes + No)*100) 

time_scripts <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(time_period, are_analysis_scripts_included_with_the_data) %>%
  mutate("Percentage of articles with reportedly available data that included analysis scripts" = Yes/(Yes + No)*100)

# Materials
time_materials_usable <- dates %>%
  filter(statement_indicates_that_materials_are == "Available") %>%
  tabyl(time_period, are_analysis_scripts_included_with_the_materials) %>%
  mutate("Percentage of articles with reportedly available materials that included material scripts" = Yes/(Yes + No)*100)

# Badge vs. data/materials accessible

# Data
data_accessible <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_data, data_statement_indicates_that_data_are) %>%
  mutate("Percentage of articles that had reportedly available data" = Available/(Available + Unavailable + `No statement`)*100)

# Materials
materials_accessible <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_materials.x, statement_indicates_that_materials_are) %>%
  mutate("Percentage of articles that had reportedly available materials" = Available/(Available + `No statement`)*100)

# Badge vs. data/materials locatable 

# Data
data_locatable <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(did_the_article_receive_a_badge_for_open_data, are_the_data_located_at_the_working_page) %>%
  mutate("Percentage of articles that reported data to be available AND had actually locatable data" = Yes/(Yes + No + `Requires permission`)*100)

materials_locatable <- dates %>%
  filter(statement_indicates_that_materials_are == "Available") %>%
  tabyl(did_the_article_receive_a_badge_for_open_materials.x, are_the_materials_located_at_the_working_page) %>%
  mutate("Percentage of articles that reported materials to be available AND had actually locatable materials" = Yes/(Yes + No + `Requires permission`)*100)

# Time vs. data/materials locatable 

# Data
data_locatable_time <- dates %>%
  filter(data_statement_indicates_that_data_are == "Available") %>%
  tabyl(time_period, are_the_data_located_at_the_working_page) %>%
  mutate("Percentage of articles that reported data to be available AND had actually locatable data" = Yes/(Yes + No + `Requires permission`)*100)

materials_locatable_time <- dates %>%
  filter(statement_indicates_that_materials_are == "Available") %>%
  tabyl(time_period, are_the_materials_located_at_the_working_page) %>%
  mutate("Percentage of articles that reported materials to be available AND had actually locatable materials" = Yes/(Yes + No + `Requires permission`)*100)

# Badge vs. codebook/scripts/understanding variables 

# Data
databadge_codebook <- dates %>%
  filter(are_the_data_located_at_the_working_page == "Yes") %>%
  tabyl(did_the_article_receive_a_badge_for_open_data, is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables) %>%
  mutate("Percentage of articles with available data that included a codebook" = Yes/(Yes + No)*100)

databadge_scripts <- dates %>%
  filter(are_the_data_located_at_the_working_page == "Yes") %>%
  tabyl(did_the_article_receive_a_badge_for_open_data, are_analysis_scripts_included_with_the_data) %>%
  mutate("Percentage of articles with available data that included analysis scripts" = Yes/(Yes + No)*100)

# Materials
materialsbadge_scripts <- dates %>%
  filter(are_the_materials_located_at_the_working_page == "Yes") %>%
  tabyl(did_the_article_receive_a_badge_for_open_materials.x, are_analysis_scripts_included_with_the_materials) %>%
  mutate("Percentage of articles with available materials that included materials scripts" = Yes/(Yes + No)*100)

