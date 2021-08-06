# 1A reliability sample articles

# Load packages 
library(tidyverse)
library(here)

# Read in scored master 1A dataset 

data_1A <- read_csv(here("data_files", "scored_master_dataset_1A.csv"))

# Randomly selecting 5 articles that Patrick coded to recode

patrick_reliability <- data_1A %>%
  filter(coder_name == "patrick mccraw") 

sample_n(patrick_reliability, size = 5)

patrick <- patrick_reliability %>%
  filter(article_id_number %in% c("6-1-2015", "18-4-2015", "10-4-2014", "15-4-2014", "9-4-2014"))

# Randomly selecting 5 articles that Helen coded to recode

helen_reliability <- data_1A %>%
  filter(coder_name == "Helen Gu") 

sample_n(helen_reliability, size = 5)

helen <- helen_reliability %>%
  filter(article_id_number %in% c("4-2-2015", "2-7-2014", "2-6-2014", "22-4-2014", "5-2-2015"))

# Randomly selecting 5 articles that Jenn coded to recode

jenn_reliability <- data_1A %>%
  filter(coder_name == "Jenn Lee") 

sample_n(jenn_reliability, size = 5)

jenn <- jenn_reliability %>%
  filter(article_id_number %in% c("7-1-2015", "5-10-2014", "12-9-2014", "1-10-2014", "5-3-2015"))

# Randomly selecting 5 articles that Georgia coded to recode

georgia_reliability <- data_1A %>%
  filter(coder_name == "Georgia Saddler") 

sample_n(georgia_reliability, size = 5)

georgia <- georgia_reliability %>%
  filter(article_id_number %in% c("8-5-2014", "14-8-2014", "9-9-2014", "10-3-2014", "1-3-2015"))

# Randomly selecting 5 articles that Will coded to recode

will_reliability <- data_1A %>%
  filter(coder_name == "Will Osmand") 

sample_n(will_reliability, size = 5)

will <- will_reliability %>%
  filter(article_id_number %in% c("24-2-2014", "7-3-2014", "5-6-2014", "5-3-2014", "5-5-2014"))

# Bind all coders' articles together

reliability_articles_1A <- rbind(patrick, helen, jenn, georgia, will) %>%
  select(coder_name:type_of_software_other)

# Export to a csv. -----

reliability_articles_1A %>% write_csv(here::here("data_files", "reliability_checking_articles_1A.csv"))
