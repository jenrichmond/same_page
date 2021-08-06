# Confirmatory 1B Analysis 

# Load packages
library(tidyverse)
library(janitor)
library(grid)
library(cowplot)
library(httr)
library(extrafont)
library(here)
library(ggeasy)
library(rstatix)

options(scipen=999) # remove scientific notation

# let's read in the data

data1B <- read_csv(here("data_files", "scored_master_dataset_1B.csv"))

glimpse(data1A)

# First factor: Subfield ------

# first, let's collapse the subfields into 4 main groups: developmental, cognition, social and 'other'
subfield_groups <- data1B %>%
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
    str_detect(article_id_number, "2019-30-7|2019-30-8|2019-30-9|2019-30-10|2019-30-11|2019-30-12") ~ "2nd half 2019",
    str_detect(article_id_number, "2020-31-1|2020-31-2|2020-31-3|2020-31-4|2020-31-5|2020-31-6") ~ "1st half 2020",
    str_detect(article_id_number, "2020-31-7|2020-31-8|2020-31-9|2020-31-10|2020-31-11|2020-31-12") ~ "2nd half 2020")) %>%
  relocate(time_period, .after = article_id_number)

glimpse(dates)

# select just the variables you need to analyse

final1B <- dates %>%
  select(article_id_number, subfield_groups, time_period, total_data_score, total_materials_score)

# check data types

glimpse(final1A)

# make sure subfield and timeperiod are factors 

final1B$subfield_groups <- as.factor(final1B$subfield_groups)
final1B$time_period <- as.factor(final1B$time_period)