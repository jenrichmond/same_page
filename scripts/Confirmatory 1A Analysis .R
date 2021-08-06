# Confirmatory 1A Analysis 

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

# Export dataset as a csv. for 6/08 lab meeting -----

final1A %>% write_csv(here::here("data_files", "clean_06-08_dataset_1A.csv"))

# ANOVA analysis - DATA ----

  # We decided to use Type II ANOVA tests because this appears to be the more conservative test out of Type I, II and III tests, and is most appropriate for our findings

data_ANOVA <- final1A %>%
  anova_test(total_data_score ~ subfield_groups * time_period)

data_ANOVA

# Simple main effects analysis showed that time_period significantly impacted Open Data Scores (p = 0.000) but subfield didn't (p = 0.085). There was no evidence of a significant interaction between the two main effects (p = 0.157). 


# ANOVA analysis - MATERIALS ------

ANOVA_materials <- final1A %>%
  anova_test(total_materials_score ~ subfield_groups * time_period)

ANOVA_materials 

# Simple main effects analysis showed that both time_period (p = 0.008) and subfield (p = 0.009) significantly impacted Open Material Scores. There was no evidence of a significant interaction between the two main effects (p = 0.530). 


# PLOTS -----

final1A <- final1A %>%
  mutate(subfield_groups = case_when(subfield_groups == "Developmental Psychology" ~ "Development", subfield_groups == "Social Psychology" ~ "Social", 
                                     TRUE ~ as.character(subfield_groups)))

# SUBFIELD X DATA SCORE - according to rstatix, insignificant

data_subfield_descriptives <- final1A %>%
  group_by(subfield_groups) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

# let's plot this

data_subfield_descriptives %>%
  ggplot(aes(x = reorder(subfield_groups, mean_data_score), y = mean_data_score, fill = subfield_groups)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_data_score - stderr, ymax = mean_data_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2) + # narrower bars
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,10), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Data Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space

# TIME PERIOD X DATA SCORE 

data_timeperiod_descriptives <- final1A %>%
  group_by(time_period) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

data_timeperiod_descriptives$time_period <- fct_relevel(data_timeperiod_descriptives$time_period, c("1st half 2014", "2nd half 2014", "1st half 2015"))

levels(data_timeperiod_descriptives$time_period)

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


# INTERACTION between time and subfield

data_subfieldtime_descriptives <- final1A %>%
  group_by(subfield_groups, time_period) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

data_subfieldtime_descriptives $time_period <- fct_relevel(data_subfieldtime_descriptives $time_period, c("1st half 2014", "2nd half 2014", "1st half 2015"))

data_subfieldtime_descriptives  %>%
  ggplot(aes(x = subfield_groups, y = mean_data_score, fill = time_period)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_data_score - stderr, ymax = mean_data_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2, # narrower bars
                position=position_dodge(.9)) + 
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,10), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Data Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm"))  # more white space



# SUBFIELD X MATERIALS SCORE 

materials_subfield_descriptives <- final1A %>%
  group_by(subfield_groups) %>%
  summarise(mean_materials_score = mean(total_materials_score, na.rm = TRUE),
            SD = sd(total_materials_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

# let's plot this

materials_subfield_descriptives %>%
  ggplot(aes(x = reorder(subfield_groups, mean_materials_score), y = mean_materials_score, fill = subfield_groups)) +
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

materials_timeperiod_descriptives$time_period <- fct_relevel(materials_timeperiod_descriptives$time_period, c("1st half 2014", "2nd half 2014", "1st half 2015"))

levels(materials_timeperiod_descriptives$time_period)

# TIME PERIOD X MATERIALS SCORE 

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

# INTERACTION between time and subfield

materials_subfieldtime_descriptives <- final1A %>%
  group_by(subfield_groups, time_period) %>%
  summarise(mean_materials_score = mean(total_materials_score, na.rm = TRUE),
            SD = sd(total_materials_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

materials_subfieldtime_descriptives $time_period <- fct_relevel(materials_subfieldtime_descriptives $time_period, c("1st half 2014", "2nd half 2014", "1st half 2015"))

materials_subfieldtime_descriptives  %>%
  ggplot(aes(x = subfield_groups, y = mean_materials_score, fill = time_period)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_materials_score - stderr, ymax = mean_materials_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2, # narrower bars
                position=position_dodge(.9)) + 
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,10), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Materials Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm"))  # more white space


# t-tests for subfields ------- (because our main effect wasn't significant, I don't know if this is valid)

# Data scores (using rstatix package)

  # Development vs. Cognition

devcog_data <- final1A %>%
  filter(subfield_groups %in% c("Development", "Cognition")) %>%
  t_test(total_data_score ~ subfield_groups)
# not significant 

  # Development vs. Social

devsocial <- final1A %>%
  filter(subfield_groups %in% c("Development", "Social")) %>%
  t_test(total_data_score ~ subfield_groups)
# significant

  # Development vs. Other

devother <- final1A %>%
  filter(subfield_groups %in% c("Development", "Other")) %>%
  t_test(total_data_score ~ subfield_groups)
# not significant 

# Material scores

  # Developmental vs. Cognition

devcog_materials <- final1A %>%
  filter(subfield_groups %in% c("Development", "Cognition")) %>%
  t_test(total_materials_score ~ subfield_groups)
# significant

  # Developmental vs. Social

devsocial_materials <- final1A %>%
  filter(subfield_groups %in% c("Development", "Social")) %>%
  t_test(total_materials_score ~ subfield_groups)
# significant

  # Developmental vs. Other

devother_materials <- final1A %>%
  filter(subfield_groups %in% c("Development", "Other")) %>%
  t_test(total_materials_score ~ subfield_groups)
# not significant

