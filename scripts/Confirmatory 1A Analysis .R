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
library(rstatix)

# let's read in the data

data1A <- read_csv(here("data_files", "scored_master_dataset_1A.csv"))

glimpse(data1A)

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
         
glimpse(dates)

# select just the variables you need to analyse

final1A <- dates %>%
  select(article_id_number, subfield_groups, time_period, total_data_score, total_materials_score)

# check data types

glimpse(final1A)

# make sure subfield and timeperiod are factors 

final1A$subfield_groups <- as.factor(final1A$subfield_groups)
final1A$time_period <- as.factor(final1A$time_period)


# now let's run an ANOVA for data scores using the jmv package

data_ANOVA <- ANOVA(formula = total_data_score ~ subfield_groups * time_period, data = final1A) 

print(data_ANOVA)

# JR I think anova from jmv can't be turned into a dataframe, there are ways to turn output into a dataframe so you can easily refer to parts of it... broom package or rstatix package might be helpful

data_ANOVA <- as.data.frame(data_ANOVA$main)

# CR let's try using the rstatix package

rstatix_output_data <- final1A %>%
  anova_test(total_data_score ~ subfield_groups * time_period)

rstatix_output_data

# huh, that's weird, according to rstatix, the only significant effect is the time period effect - why are they different?


# ANOVA for materials

materials_ANOVA <- ANOVA(formula = total_materials_score ~ subfield_groups * time_period, data = dates) 

print(materials_ANOVA)

materialsANOVA <- as.data.frame(materials_ANOVA$main)

# let's try using the rstatix package

rstatix_output_materials <- final1A %>%
  anova_test(total_materials_score ~ subfield_groups * time_period)

# again, the stats are different


## JENNY UP TO HERE--- LOOKS GOOD TO ME... MAIN EFFECTS OF SUBFIELD AND TIMEPOINT, BUT NO INTERACTIONS
## WHAT DOES THAT MEAN... YOU NEED SOME PLOTS :)

# Christina attempting to plot

# SUBFIELD X DATA SCORE - according to rstatix, insignificant

data_subfield_descriptives <- final1A %>%
  group_by(subfield_groups) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

# let's plot this

data_subfield_descriptives %>%
  ggplot(aes(x = subfield_groups, y = mean_data_score, fill = subfield_groups)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_data_score - stderr, ymax = mean_data_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2) + # narrower bars
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,6), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Data Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space

# TIME PERIOD X DATA SCORE - according to rstatix, significant 

data_timeperiod_descriptives <- final1A %>%
  group_by(time_period) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

# let's plot this

data_timeperiod_descriptives %>%
  ggplot(aes(x = time_period, y = mean_data_score, fill = time_period)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_data_score - stderr, ymax = mean_data_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2) + # narrower bars
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,8), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Time Period", y = "Mean Open Data Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space

# SUBFIELD X MATERIALS SCORE - according to rstatix, significant

materials_subfield_descriptives <- final1A %>%
  group_by(subfield_groups) %>%
  summarise(mean_materials_score = mean(total_materials_score, na.rm = TRUE),
            SD = sd(total_materials_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

# let's plot this

materials_subfield_descriptives %>%
  ggplot(aes(x = subfield_groups, y = mean_materials_score, fill = subfield_groups)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_materials_score - stderr, ymax = mean_materials_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2) + # narrower bars
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,6), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Materials Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space

# TIME PERIOD X MATERIALS SCORE - according to rstatix, significant 

materials_timeperiod_descriptives <- final1A %>%
  group_by(time_period) %>%
  summarise(mean_materials_score = mean(total_materials_score, na.rm = TRUE),
            SD = sd(total_materials_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

# let's plot this

materials_timeperiod_descriptives %>%
  ggplot(aes(x = time_period, y = mean_materials_score, fill = time_period)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_materials_score - stderr, ymax = mean_materials_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2) + # narrower bars
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,6), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Time Period", y = "Mean Open Data Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space


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

