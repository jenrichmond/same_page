# Cleaning Master Datasets for OSF

## 1A

master_1A <- read_csv(here("data_files", "master_dataset_1A.csv"))

clean_master_1A <- master_1A %>%
  select(article_id_number:url_to_supplemental_information) %>%
  select(-journal)

clean_master_1A %>% write_csv(here::here("data_files", "Study 1A Master Dataset.csv"))


## 1B

master_1B <- read_csv(here("data_files", "master_dataset_1B.csv"))

clean_master_1B <- master_1B %>%
  select(article_id_number:URL_supplemental_info) 

clean_master_1B %>% write_csv(here::here("data_files", "Study 1B Master Dataset.csv"))