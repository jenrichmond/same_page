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
library("vcd")
library(irr)

# Read in Reliability Checking data

data <- read_csv(here("data_files", "data1A_reliability_checking")) %>%
  select(coder_name, article_id_number_and_title, id_number = check_id, participants:topic_other)

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

# let's make the data long

subfield_long <- psyc_subfield %>%
  select(coder_name, id_number, subfield) %>%
  pivot_wider(names_from = coder_name, values_from = subfield) 

# let's take out the id column so R doesn't think "ID" is a rater
subfield_clean <- subfield_long %>%
  select("Helen Gu":"Will Osmand")

# let's replace the NAs with blanks (I think this was the root of the problems I was facing)

subfield_clean_NA <- subfield_clean %>%
  mutate(`Helen Gu` = coalesce(`Helen Gu`, "")) %>%
  mutate(`Georgia Saddler` = coalesce(`Georgia Saddler`, "")) %>%
  mutate(`patrick mccraw` = coalesce(`patrick mccraw`, "")) %>%
  mutate(`Jenn Lee` = coalesce(`Jenn Lee`, "")) %>%
  mutate(`Will Osmand` = coalesce(`Will Osmand`, "")) 

# now we can run the reliability analysis

# not sure which kappa is the right one to use

# fleiss - used for multiple categorical variables
  # https://www.datanovia.com/en/lessons/fleiss-kappa-in-r-for-multiple-categorical-variables/#interpretation-magnitude-of-the-agreement 

kappam.fleiss(subfield_clean_NA)

# If I'm understanding correctly, according to this website, we have fair to good agreement, above chance

# light - average of all possible two-raters kappa
  # https://www.datanovia.com/en/lessons/cohens-kappa-in-r-for-two-categorical-variables/#kappa-for-more-than-two-raters  
kappam.light(subfield_clean_NA)
  # again, fair to good agreement

# YAY this seems to work, ignore rest of code





# so it looks like the number of raters is correct, but the subjects isn't

# ATTEMPT 2

# let's assign each subfield a number
  # Behavioural Neuroscience = 1
  # Developmental Psychology = 2
  # Cognitive Neuroscience = 3
  # Perception = 4
  # Social Psychology = 5
  # Cognition = 6
  # Health Psychology = 7

Christina = c(6, 5, 6, 2, 7)
Helen = c(6, 5, 6, 2)
Georgia = c(6, 5, 6, 2)
Patrick = c(6, 5, 3, 2)
Jenn = c(6, 6, 6, 2, 7)
Will = c(6, 5)

subfield_relaibility <- as.table(rbind(Christina, Helen, Georgia, Patrick, Jenn, Will))

subfield_reliability <- as.data.frame("subfield_relaibility")

categories <- c("Behavioural Neuroscience", "Developmental Psychology", "Cognitive Neuroscience", "Perception", "Social Psychology", "Cognition", "Health Psychology")

dimnames(subfield_relaibility) <- list

# ATTEMPT 3

1A1 = c(0, 0, 0, 0, 0, 6, 0)
1A2 = c(0, 0, 0, 0, 5, 1, 0)
1A3 = c(0, 0, 1, 0, 0, 4, 0)
1A4 = c(0, 5, 0, 0, 0, 0, 0)
1A5 = c(0, 0, 0, 0, 0, 0, 2)

subfield <- as.table(rbind(
  c(0, 0, 0, 0, 0, 6, 0), c(0, 0, 0, 0, 5, 1, 0),
  c(0, 0, 1, 0, 0, 4, 0), c(0, 5, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 2)))

categories <- c("Behavioural Neuroscience", "Developmental Psychology", "Cognitive Neuroscience", "Perception", "Social Psychology", "Cognition", "Health Psychology")

subfield <- as.data.frame(rbind(
  c(0, 0, 0, 0, 0, 6, 0), c(0, 0, 0, 0, 5, 1, 0),
  c(0, 0, 1, 0, 0, 4, 0), c(0, 5, 0, 0, 0, 0, 0),
  c(0, 0, 0, 0, 0, 0, 2))) 

%>%
  rename(V1 == "BN", V2 == "Developmental", V3 == "CN", V4 == "Perception", V5 == "Social", V6 == "Cognition", V7 == "Health")

kappam.fleiss(subfield)

# the number of subjects is correct, but the number of raters isn't (one extra)

# ATTEMPT 4

subfield_coding <- as.data.frame(rbind(Christina = c(6, 5, 6, 2, 7), Helen = c(6, 5, 6, 2), Georgia = c(6, 5, 6, 2), Patrick = c(6, 5, 3, 2), Jenn = c(6, 6, 6, 2, 7), Will = c(6, 5)))

# Nope, this is still the wrong format

# ATTEMPT 5

# let's assign each subfield a number
# Behavioural Neuroscience = 1
# Developmental Psychology = 2
# Cognitive Neuroscience = 3
# Perception = 4
# Social Psychology = 5
# Cognition = 6
# Health Psychology = 7

subfield_long_recoded <- subfield_long %>%
  recode('Behavioural Neuroscience' = "1")
  select(case_when("Helen Gu" == "Behavioural Neuroscience" ~ 1, "Helen Gu" == "Developmental Psychology" ~ 2, "Helen Gu"" == "Cognitive Neuroscience"" ~ 3, "Helen Gu" == "Perception" ~ 4, "Helen Gu"" == "Social Psychology" ~ 5, "Helen Gu" == "Cognition" ~ 6, "Helen Gu"" == "Health Psychology"))
  
  # trying to recode the subfields as numbers so that I can put in this format
    # each row is a Check article
    # each column is a coder
  




