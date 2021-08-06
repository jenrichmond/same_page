# 1B reliability sample articles

# Load packages 
library(tidyverse)
library(here)

# Read in scored master 1A dataset 

data_1B <- read_csv(here("data_files", "scored_master_dataset_1B.csv"))

# Randomly selecting 5 articles that Patrick coded to recode

patrick_reliability <- data_1B %>%
  filter(coder_name == "patrick mccraw") 

sample_n(patrick_reliability, size = 5)

patrick <- patrick_reliability %>%
  filter(article_id_number %in% c("2020-31-6-729", "2020-31-3-268", "2020-31-3-293", "2019-30-10-1460", "2020-31-8-1001"))

# Randomly selecting 5 articles that Helen coded to recode

helen_reliability <- data_1B %>%
  filter(coder_name == "Helen Gu") 

sample_n(helen_reliability, size = 5)

helen <- helen_reliability %>%
  filter(article_id_number %in% c("2020-31-7-873", "2020-31-7-848", "2020-31-7-881", "2020-31-8-944", "2019-30-8-1123"))

# Jenn - it just so happens that all the article Jenn coded (n = 8) were corrigenda (i.e. non-empirical), so none of her codings are present in this dataset

# Randomly selecting 5 articles that Georgia coded to recode

georgia_reliability <- data_1B %>%
  filter(coder_name == "Georgia Saddler") 

sample_n(georgia_reliability, size = 5)

georgia <- georgia_reliability %>%
  filter(article_id_number %in% c("2020-31-10-1283", "2019-30-7-1030", "2020-31-6-607", "2020-31-3-280", "2020-31-5-505"))

# Randomly selecting 5 articles that Will coded to recode

will_reliability <- data_1B %>%
  filter(coder_name == "Will Osmand") 

sample_n(will_reliability, size = 4)
  # Will actually ended up coded only 4 empirical articles (in the whole dataset)

will <- will_reliability %>%
  filter(article_id_number %in% c("2019-30-10-1522", "2019-30-10-1483", "2020-31-5-592", "2020-31-9-1191"))

# Bind all coders' articles together

reliability_articles_1B <- rbind(patrick, helen, georgia, will) %>%
  select(coder_name:type_of_software_other)

# Export to a csv. -----

reliability_articles_1B %>% write_csv(here::here("data_files", "reliability_checking_articles_1B.csv"))


