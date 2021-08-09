# ANOVAs on ALL data 

# Load packages
library(tidyverse)
library(janitor)
library(here)

# Read in 1B dataset ----

data1B <- read_csv(here("data_files", "scored_master_dataset_1B.csv"))

# Select only relevant data 

data1Bselect <- data1B %>%
  select(article_id_number, subfield, total_data_score, total_materials_score)

# Read in 1A dataset ----

data1A <- read_csv(here("data_files", "scored_master_dataset_1A.csv"))

# Select only relevant data 

data1Aselect <- data1A %>%
  select(article_id_number, subfield, total_data_score, total_materials_score)

# let's merge the two datasets

alldata <- rbind(data1Aselect, data1Bselect)

# First factor: Subfield ----

# let's let's collapse the subfields into 4 main groups: developmental, cognition, social and 'other'
subfield_groups <- alldata %>%
  mutate(subfield_groups = case_when(subfield == "Behavioural Neuroscience" ~ "Other",
                                     subfield == "Cognitive Neuroscience" ~ "Other",
                                     subfield == "Health Psychology" ~ "Other",
                                     subfield == "Perception" ~ "Other",
                                     subfield == "Developmental Psychology" ~ "Development",
                                     subfield == "Social Psychology" ~  "Social",
                                     TRUE ~ as.character(subfield))) %>%
  relocate(subfield_groups, .after = subfield)

# we can now delete the original subfield column
subfield_groups <- subfield_groups %>%
  select(-subfield)

# Second factor: Time -----

dates <- subfield_groups %>%
  mutate(time_period = case_when(
    str_detect(article_id_number, "1-2014|2-2014|3-2014|4-2014|5-2014|6-2014") ~ "1st half 2014",
    str_detect(article_id_number, "7-2014|8-2014|9-2014|10-2014|11-2014|12-2014") ~ "2nd half 2014",
    str_detect(article_id_number, "1-2015|2-2015|3-2015|4-2015|5-2015") ~ "1st half 2015",
    str_detect(article_id_number, "2019-30-7|2019-30-8|2019-30-9|2019-30-10|2019-30-11|2019-30-12") ~ "2nd half 2019",
    str_detect(article_id_number, "2020-31-1|2020-31-2|2020-31-3|2020-31-4|2020-31-5|2020-31-6") ~ "1st half 2020",
    str_detect(article_id_number, "2020-31-7|2020-31-8|2020-31-9|2020-31-10|2020-31-11|2020-31-12") ~ "2nd half 2020")) %>%
  relocate(time_period, .after = article_id_number)

# make sure subfield and timeperiod are factors 

dates$subfield_groups <- as.factor(dates$subfield_groups)
dates$time_period <- as.factor(dates$time_period)

# DATA ANOVA -----

data_ANOVA <- dates %>%
  anova_test(total_data_score ~ subfield_groups * time_period)

data_ANOVA

# MATERIALS ANOVA ----

materials_ANOVA <- dates %>%
  anova_test(total_materials_score ~ subfield_groups * time_period)

materials_ANOVA

# PLOTS -----

# Subfield x Data score

data_subfield_descriptives <- dates %>%
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
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Data Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space

# Time period x Data score

data_timeperiod_descriptives <- dates %>%
  group_by(time_period) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

data_timeperiod_descriptives$time_period <- fct_relevel(data_timeperiod_descriptives$time_period, c("1st half 2014", "2nd half 2014", "1st half 2015", "2nd half 2019", "1st half 2020", "2nd half 2020"))

levels(data_timeperiod_descriptives$time_period)

# let's plot this

data_timeperiod_descriptives %>%
  ggplot(aes(x = time_period, y = mean_data_score, fill = time_period)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_data_score - stderr, ymax = mean_data_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2) + # narrower bars
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,21), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Time Period", y = "Mean Open Data Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space

# Interaction between subfield and time - Data Scores

data_subfieldtime_descriptives <- dates %>%
  group_by(subfield_groups, time_period) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

data_subfieldtime_descriptives $time_period <- fct_relevel(data_subfieldtime_descriptives $time_period, c("1st half 2014", "2nd half 2014", "1st half 2015", "2nd half 2019", "1st half 2020", "2nd half 2020"))

data_subfieldtime_descriptives  %>%
  ggplot(aes(x = subfield_groups, y = mean_data_score, fill = time_period)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_data_score - stderr, ymax = mean_data_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2, # narrower bars
                position=position_dodge(.9)) + 
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Data Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm"))  # more white space

# Subfield x Materials score

materials_subfield_descriptives <- dates %>%
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
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Materials Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space

# Time period x Materials score

materials_timeperiod_descriptives <- dates %>%
  group_by(time_period) %>%
  summarise(mean_materials_score = mean(total_materials_score, na.rm = TRUE),
            SD = sd(total_materials_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

materials_timeperiod_descriptives$time_period <- fct_relevel(materials_timeperiod_descriptives$time_period, c("1st half 2014", "2nd half 2014", "1st half 2015", "2nd half 2019", "1st half 2020", "2nd half 2020"))

levels(materials_timeperiod_descriptives$time_period)

# let's plot this

materials_timeperiod_descriptives %>%
  ggplot(aes(x = time_period, y = mean_materials_score, fill = time_period)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_materials_score - stderr, ymax = mean_materials_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2) + # narrower bars
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,21), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_remove_legend() +
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Time Period", y = "Mean Open Materials Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm")) # more white space

# Interaction between subfield and time - Materials Scores

materials_subfieldtime_descriptives <- dates %>%
  group_by(subfield_groups, time_period) %>%
  summarise(mean_materials_score = mean(total_materials_score, na.rm = TRUE),
            SD = sd(total_materials_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

materials_subfieldtime_descriptives $time_period <- fct_relevel(materials_subfieldtime_descriptives $time_period, c("1st half 2014", "2nd half 2014", "1st half 2015", "2nd half 2019", "1st half 2020", "2nd half 2020"))

materials_subfieldtime_descriptives  %>%
  ggplot(aes(x = subfield_groups, y = mean_materials_score, fill = time_period)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_materials_score - stderr, ymax = mean_materials_score + stderr), # specifying what the standard error is
                size=.3, # thinner lines
                width=.2, # narrower bars
                position=position_dodge(.9)) + 
  theme_classic() + # white background
  scale_y_continuous(limits = c(0,25), expand = c(0,0)) + # getting the bars to start at the bottom of the graph
  easy_all_text_size(size = 9) + # change the size of the text
  easy_labs(x = "Subfield", y = "Mean Open Materials Score") + # change the x and y labels
  theme(plot.margin=unit(c(1,1,1,1),"cm"))  # more white space

