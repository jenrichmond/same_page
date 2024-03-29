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
library(apa)
library(ez)
library(afex)

options(scipen=999) # remove scientific notation

# let's read in the data

data1B <- read_csv(here("data_files", "scored_master_dataset_1B.csv"))

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

glimpse(final1B)

# make sure subfield and timeperiod are factors 

final1B$subfield_groups <- as.factor(final1B$subfield_groups)
final1B$time_period <- as.factor(final1B$time_period)

# ANOVA analysis - DATA ----

# We decided to use Type II ANOVA tests because this appears to be the more conservative test out of Type I, II and III tests, and is most appropriate for our findings

data_ANOVA <- final1B %>%
  anova_test(total_data_score ~ subfield_groups * time_period)

data_ANOVA

# There was a significant main effect of time period (F(df) = X, p <pvalues) and subfield (F(df) = X, p <pvalues), however the interaction between time period and subfield (F(df) = X, p <pvalues) were not statistically significant. 

# For R-markdown
  # Our two-way between-subjects ANOVA generated a significant main effect of subfield, F(`r data_ANOVA$DFn[1]`, `r data_ANOVA$DFd[1]`) = `r data_ANOVA$F[1]`, p = `r data_ANOVA$p[1]` and time period, F(r data_ANOVA$DFn[2]`, `r data_ANOVA$DFd[2]`) = `r data_ANOVA$F[2]`, p = `r data_ANOVA$p[2]`., and subfield, F(`r data_ANOVA$DFn[1]`, `r data_ANOVA$DFd[1]`) = `r data_ANOVA$F[1]`, p = `r data_ANOVA$p[1]`.
  # However, the interaction between subfield and time period, F(`r data_ANOVA$DFn[3]`, `r data_ANOVA$DFd[3]`) = `r data_ANOVA$F[3]`, was not statistically significant.

# Alternatively: 

ez_ANOVA_data <- ezANOVA(final1B, dv = total_data_score, wid = article_id_number, between = c(time_period, subfield_groups), detailed = TRUE)

anova_apa(ez_ANOVA_data)

# Christina tried using the aov_ez package, however, it appears this doesn't run a Type 2 ANOVA. 
  x <- aov_ez(id = "article_id_number", dv = "total_data_score", data = final1B, between = c("time_period", "subfield_groups"), detailed = TRUE)
  anova_apa(x)
  
# The ezANOVA (above) package does run a Type 2 ANOVA

# For R-markdown

# apa print options from papaja

#### estimate
`r ez_ANOVA_data$estimate`

### statistic
`r ez_ANOVA_data$statistic`

### full_result 
`r ez_ANOVA_data$full_result`

### table
`r ez_ANOVA_data$table`

# Christina confused about how to integrate these numbers into a text format

# ANOVA analysis - MATERIALS ------

ANOVA_materials <- final1B %>%
  anova_test(total_materials_score ~ subfield_groups * time_period)

ANOVA_materials 

# There was a significant main effect of time period (F(df) = X, p <pvalues) and subfield (F(df) = X, p <pvalues), however the interaction between time period and subfield (F(df) = X, p <pvalues) were not statistically significant. 

# For R-markdown
  # Our two-way between-subjects ANOVA generated a significant main effect of subfield, F(r ANOVA_materials$DFn[1]`, `r ANOVA_materials$DFd[1]`) = `r ANOVA_materials$F[1]`, p = `r materials_ANOVA$p[1]`.
  # However, the main effect of subfield, F(`r data_ANOVA$DFn[2]`, `r data_ANOVA$DFd[2]`) = `r data_ANOVA$F[2]`, p = `r data_ANOVA$p[2]` and the interaction between subfield and time period, F(`r data_ANOVA$DFn[3]`, `r data_ANOVA$DFd[3]`) = `r data_ANOVA$F[3]`, was not statistically significant.

# Alternatively: 

ez_ANOVA_materials <- ezANOVA(final1B, dv = total_materials_score, wid = article_id_number, between = c(time_period, subfield_groups), detailed = TRUE)

anova_apa(ez_ANOVA_materials)

# PLOTS -----

final1B <- final1B %>%
  mutate(subfield_groups = case_when(subfield_groups == "Developmental Psychology" ~ "Development", subfield_groups == "Social Psychology" ~ "Social", 
                                     TRUE ~ as.character(subfield_groups)))

# SUBFIELD X DATA SCORE 

data_subfield_descriptives <- final1B %>%
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

# TIME PERIOD X DATA SCORE 

data_timeperiod_descriptives <- final1B %>%
  group_by(time_period) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

data_timeperiod_descriptives$time_period <- fct_relevel(data_timeperiod_descriptives$time_period, c("2nd half 2019", "1st half 2020", "2nd half 2020"))

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


# INTERACTION between time and subfield

data_subfieldtime_descriptives <- final1B %>%
  group_by(subfield_groups, time_period) %>%
  summarise(mean_data_score = mean(total_data_score, na.rm = TRUE),
            SD = sd(total_data_score, na.rm = TRUE),
            N = n(),
            stderr = SD/sqrt(N))

data_subfieldtime_descriptives $time_period <- fct_relevel(data_subfieldtime_descriptives $time_period, c("2nd half 2019", "1st half 2020", "2nd half 2020"))

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

# t-tests for subfields ------- 

# Data scores (using rstatix package)

# don't know if you need this, try running t-tests without
final1B$article_id_number <- as.character(final1B$article_id_number)
glimpse(final1B)

# Development vs. Cognition

devcog_data <- final1B %>%
  filter(subfield_groups %in% c("Developmental Psychology", "Cognition")) %>%
  t_test(total_data_score ~ subfield_groups)
# not significant 

# Development vs. Social

devsocial <- final1B %>%
  filter(subfield_groups %in% c("Developmental Psychology", "Social Psychology")) %>%
  t_test(total_data_score ~ subfield_groups)
# significant

# Development vs. Other

devother <- final1B %>%
  filter(subfield_groups %in% c("Developmental Psychology", "Other")) %>%
  t_test(total_data_score ~ subfield_groups)
# not significant 

# Material scores

# Developmental vs. Cognition

devcog_materials <- final1B %>%
  filter(subfield_groups %in% c("Developmental Psychology", "Cognition")) %>%
  t_test(total_materials_score ~ subfield_groups)
# significant

# Developmental vs. Social

devsocial_materials <- final1B %>%
  filter(subfield_groups %in% c("Developmental Psychology", "Social Psychology")) %>%
  t_test(total_materials_score ~ subfield_groups)
# significant

# Developmental vs. Other

devother_materials <- final1B %>%
  filter(subfield_groups %in% c("Developmental Psychology", "Other")) %>%
  t_test(total_materials_score ~ subfield_groups)
# not significant









                               