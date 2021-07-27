# Confirmatory 1A Analysis 

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
  mutate(dates = case_when(
    str_detect(article_id_number, "1-2014|2-2014|3-2014|4-2014|5-2014|6-2014") ~ "1st half 2014",
    str_detect(article_id_number, "7-2014|8-2014|9-2014|10-2014|11-2014|12-2014") ~ "2nd half 2014",
    str_detect(article_id_number, "1-2015|2-2015|3-2015|4-2015|5-2015") ~ "1st half 2015")) %>%
  relocate(dates, .after = article_id_number)
         
# now let's run an ANOVA for data scores

# using jmv pacakage - repeated measured ANOVA
  # need Jenny's help here - https://rdrr.io/cran/jmv/man/anovaRM.html 

anovaRM <- dates %>%
  anovaRM(data = dates,
        rm = list(list(label = 'subfield_groups',
                       levels = c('Developmental Psychology', 'Cognition', 'Social Psychology', 'Other'))),
        rmCells = list(list(measure = 'dates',
                            cell = '1st half 2014', '2nd half 2014', '1st half 2015')),
        depLabel = 'total_data_score')


#using afex package - factorial analysis 
  # https://cran.r-project.org/web/packages/afex/afex.pdf - page 27
  # https://www.rdocumentation.org/packages/afex/versions/1.0-1 

afex_data_1 <- data %>%
  aov_ez(id = "article_id_number", dv = "total_data_score", within = c("subfield_groups", "dates"), data = dates)
# I get this error: 'Error in if (make.names(name) != name) { : argument is of length zero'

summary(afex_data_1)

# I tried using the code below but I don't think I was on the right track

# apparently the data needs to be a long format

# first let's make the data and material scores character variables

character_variables <- transform(dates, "total_data_score" = as.character(total_data_score)) %>%
  transform(dates, "total_materials_score" = as.character(total_materials_score))

# now we can make the data long

longer_data <- character_variables %>%
  select(article_id_number, subfield_groups, dates, total_data_score) %>%
  pivot_longer(names_to = "factor", values_to = "output", subfield_groups:total_data_score)

# and now we can run the factorial analysis 

afex_data <- longer_data %>%
  aov_ez("article_id_number", "total_data_score", longer_data, between = NULL, within = c("dates", "subfield_groups"), observered = c("dates", "subfield_groups"))





# Christina having a go at some exploratory analyses 
  # for now, I've just used percentages - I'm not sure if there's a more technical method we want to use

# Subfield vs. badges

# Data
subfield_databadges <- dates %>%
  tabyl(subfield_groups, did_the_article_receive_a_badge_for_open_data) %>%
  mutate(percent = Yes/sum(Yes + No)*100)

# Materials
subfield_materialsbadges <- dates %>%
  tabyl(subfield_groups, did_the_article_receive_a_badge_for_open_materials.x) %>%
  mutate(percent = Yes/sum(Yes + No)*100)

# Subfield vs. codebook/scripts/understanding variables

# Data
subfield_codebook <- dates %>%
  tabyl(subfield_groups, is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables) %>%
  mutate(percent = Yes/sum(Yes + No)*100)
  # percentage here is of the articles which have data available

subfield_scripts <- dates %>%
  tabyl(subfield_groups, are_analysis_scripts_included_with_the_data) %>%
  mutate(percent = Yes/sum(Yes + No)*100)
  # percentage here is of the articles which have data available

# Materials
subfield_materials_usable <- dates %>%
  tabyl(subfield_groups, are_analysis_scripts_included_with_the_materials) %>%
  mutate(percent = Yes/sum(Yes + No)*100)
# percentage here is of the articles which have data available

# Badge vs. data/materials accessible

# Data
data_accessible <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_data, data_statement_indicates_that_data_are) %>%
  mutate(percent = Available/sum(Available + Unavailable )*100)
# percentage here is of the articles that had an availability statement

# Materials
materials_accessible <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_materials.x, statement_indicates_that_materials_are) %>%
  mutate(percent = Available/sum(Available)*100)
# percentage here is of the articles that had an availability statement
# VERY interesting finding

# Badge vs. data/materials locatable 

# Data
data_locatable <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_data, are_the_data_located_at_the_working_page) %>%
  mutate(percent = Yes/sum(Yes + No + `Requires permission`)*100)

materials_locatable <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_materials.x, are_the_materials_located_at_the_working_page) %>%
  mutate(percent = Yes/sum(Yes + No + `Requires permission`)*100)
# a little confused with the 'Yes' percentage here

# Badge vs. codebook/scripts/understanding variables

# Data
databadge_codebook <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_data, is_a_codebook_included_with_the_data_or_other_means_of_understanding_the_variables) %>%
  mutate(percent = Yes/sum(Yes + No)*100)
# percentage here is of the articles which have data available

databadge_scripts <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_data, are_analysis_scripts_included_with_the_data) %>%
  mutate(percent = Yes/sum(Yes + No)*100)
# percentage here is of the articles which have data available

# Materials
materialsbadge_scripts <- dates %>%
  tabyl(did_the_article_receive_a_badge_for_open_materials.x, are_analysis_scripts_included_with_the_materials) %>%
  mutate(percent = Yes/sum(Yes + No)*100)
# percentage here is of the articles which have data available

