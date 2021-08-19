# Load packages
library(tidyverse)
library(janitor)
library(here)
library(Hmisc)
library(vcd)
library(irr)
library(psych)

# guidelines suggested by Fleiss (1981): 
  #40–.60 is fair, 
  #.60–.75 is good, 
  #and over .75 is excellent.

# GOLD STANDARD -----

# Read in Christina's Reliability Checking data - gold standard

gold_standard_data <- read_csv(here("data_files", "data1A_reliability_checking")) %>%
  select(coder_name, article_id_number, participants:topic_other)

# Let's assign the articles to a subfield

gold_psyc_subfield <- gold_standard_data %>%
  mutate(subfield = case_when("Animals" == participants ~ "Other", 
                              "Humans" == participants & "0-18 years or 65 years+" == age ~ "Developmental Psychology", 
                              "Humans" == participants & "Brain" == brain_beh  ~ "Other",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Sensation" == topic ~ "Other",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Emotion, personality, social behaviour" == topic ~ "Social Psychology",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Intelligence, memory, decision making, reasoning, language, problem solving, creative thinking" == topic ~ "Cognition",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Fitness, weight, consumption, hormone levels, chemical uptake, sleeping patterns" == topic ~ "Other")) %>%
  relocate(subfield, .after = article_id_number)

# let's make the data wide

gold_subfield_wide <- gold_psyc_subfield %>%
  select(coder_name, article_id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) 

# CODERS DATA -----

# let's load in coder's reliability checking data

data <- read_csv(here("data_files", "scored_master_dataset_1A.csv")) %>%
  select(coder_name, article_id_number, participants:topic_other) %>%
  filter(article_id_number %in% c("10-4-2014", "15-4-2014", "18-4-2015", "6-1-2015", "9-4-2014", "2-6-2014", "2-7-2014", "22-4-2014", "4-2-2015", "5-2-2015", "1-10-2014", "12-9-2014", "5-10-2014", "5-3-2015", "7-1-2015", "1-3-2015", "10-3-2014", "14-8-2014", "8-5-2014", "9-9-2014", "24-2-2014", "5-3-2014", "5-5-2014", "5-6-2014", "7-3-2014"))

psyc_subfield <- data %>%
  mutate(subfield = case_when("Animals" == participants ~ "Other", 
                              "Humans" == participants & "0-18 years or 65 years+" == age ~ "Developmental Psychology", 
                              "Humans" == participants & "Brain" == brain_beh  ~ "Other",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Sensation" == topic ~ "Other",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Emotion, personality, social behaviour" == topic ~ "Social Psychology",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Intelligence, memory, decision making, reasoning, language, problem solving, creative thinking" == topic ~ "Cognition",
                              "Humans" == participants & brain_beh %in% c("Both", "Behaviour") & "Fitness, weight, consumption, hormone levels, chemical uptake, sleeping patterns" == topic ~ "Other")) %>%
  relocate(subfield, .after = article_id_number)

# GENERAL RELIABILITY ----

# let's make the coders' data wide
coders_wide <- psyc_subfield  %>%
  select(coder_name, article_id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) 

# let's fill in some data 
coders_wide <- coders_wide %>%
  mutate(`Helen Gu` = case_when(article_id_number == "2-7-2014" ~ "Social Psychology", article_id_number == "5-2-2015" ~ "Social Psychology",
                              TRUE ~ as.character(`Helen Gu`))) %>% # Helen put 'Other' for these two articles. Christina checked what she wrote in the 'Other' box and came to these conclusions.
  mutate(`Georgia Saddler` = case_when(article_id_number == "10-3-2014" ~ "Social Psychology",
                              TRUE ~ as.character(`Georgia Saddler`))) # Georgia put 'Other' for this article. Christina checked what she wrote in the 'Other' box and came to this conclusion.

# merge gold standard with coders' data
gold_overall_reliability <- merge(coders_wide, gold_subfield_wide, by = "article_id_number")

# let's take out the id column so R doesn't think "ID" is a rater
gold_overall_reliability <- gold_overall_reliability %>%
  select(-article_id_number, `Jenn Lee`:`Will Osmand`, gold_standard = `Christina Rochios`)

# let's merge the coders data into one column
overall_reliability <- gold_overall_reliability %>%
  unite("coders_data", `Jenn Lee`:`Will Osmand`, remove = TRUE, na.rm = TRUE) %>%
  select(gold_standard, coders_data)

# let's run the kappa reliability analysis 
kappa2(overall_reliability)

# GOOD general reliability 

# agreement of coders
agree(overall_reliability, tolerance=0)

# JENN'S RELIABILITY ----

# let's filter for Jenn's data alone and make it long
jenn <- psyc_subfield %>%
  filter(coder_name == "Jenn Lee") %>%
  select(coder_name, article_id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) 

# filter Christina's data so it only has article's Jenn coded
christina_jenn <- gold_subfield_long %>%
  filter(article_id_number %in% c("1-10-2014", "12-9-2014", "5-10-2014", "5-3-2015", "7-1-2015"))

christina_jenn_reliability <- merge(christina_jenn, jenn, by = "article_id_number")

# let's take out the id column so R doesn't think "ID" is a rater
christina_jenn_reliability <- christina_jenn_reliability %>%
  select(-article_id_number)

# kappa reliability analysis 
kappa2(christina_jenn_reliability)

# EXCELLENT reliability

# HELEN'S RELIABILITY ----

# let's filter for Helen's data alone and make it long
helen <- psyc_subfield %>%
  filter(coder_name == "Helen Gu") %>%
  select(coder_name, article_id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) %>%
  mutate(`Helen Gu` = case_when(article_id_number == "2-7-2014" ~ "Social Psychology", article_id_number == "5-2-2015" ~ "Social Psychology",
                                TRUE ~ as.character(`Helen Gu`))) # Helen put 'Other' for these two articles. Christina checked what she wrote in the 'Other' box and came to these conclusions.

# filter Christina's data so it only has article's Helen coded
christina_helen <- gold_subfield_long %>%
  filter(article_id_number %in% c("2-6-2014", "2-7-2014", "22-4-2014", "4-2-2015", "5-2-2015"))

christina_helen_reliability <- merge(christina_helen, helen, by = "article_id_number")

# let's take out the id column so R doesn't think "ID" is a rater
christina_helen_reliability <- christina_helen_reliability %>%
  select(-article_id_number)

# kappa reliability analysis 
kappa2(christina_helen_reliability)

# FAIR reliability

# PATRICK'S RELIABILITY ----

# let's filter for Helen's data alone and make it long
patrick <- psyc_subfield %>%
  filter(coder_name == "patrick mccraw") %>%
  select(coder_name, article_id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) 

# filter Christina's data so it only has article's patrick coded
christina_patrick <- gold_subfield_long %>%
  filter(article_id_number %in% c("10-4-2014", "15-4-2014", "18-4-2015", "6-1-2015", "9-4-2014"))

christina_patrick_reliability <- merge(christina_patrick, patrick, by = "article_id_number")

# let's take out the id column so R doesn't think "ID" is a rater
christina_patrick_reliability <- christina_patrick_reliability %>%
  select(-article_id_number)

# kappa reliability analysis 
kappa2(christina_patrick_reliability)

# POOR reliability

# GEORGIA'S RELIABILITY ----

# let's filter for Helen's data alone and make it long
georgia <- psyc_subfield %>%
  filter(coder_name == "Georgia Saddler") %>%
  select(coder_name, article_id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) %>%
  mutate(`Georgia Saddler` = case_when(article_id_number == "10-3-2014" ~ "Social Psychology",
                             TRUE ~ as.character(`Georgia Saddler`))) # Georgia put 'Other' for this article. Christina checked what she wrote in the 'Other' box and came to this conclusion.


# filter Christina's data so it only has article's Georgia coded
christina_georgia <- gold_subfield_long %>%
  filter(article_id_number %in% c("1-3-2015", "10-3-2014", "14-8-2014", "8-5-2014", "9-9-2014"))

christina_georgia_reliability <- merge(christina_georgia, georgia, by = "article_id_number")

# let's take out the id column so R doesn't think "ID" is a rater
christina_georgia_reliability <- christina_georgia_reliability %>%
  select(-article_id_number)

# kappa reliability analysis 
kappa2(christina_georgia_reliability)

# GOOD reliability

# WILL'S RELIABILITY ----

# let's filter for Helen's data alone and make it long
will <- psyc_subfield %>%
  filter(coder_name == "Will Osmand") %>%
  select(coder_name, article_id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) 

# filter Christina's data so it only has article's Will coded
christina_will <- gold_subfield_long %>%
  filter(article_id_number %in% c("24-2-2014", "5-3-2014", "5-5-2014", "5-6-2014", "7-3-2014"))

christina_will_reliability <- merge(christina_will, will, by = "article_id_number")

# let's take out the id column so R doesn't think "ID" is a rater
christina_will_reliability <- christina_will_reliability %>%
  select(-article_id_number)

# kappa reliability analysis 
kappa2(christina_will_reliability)

# POOR reliability

# Overall, 1A inter-rater reliability ranges from poor to excellent






