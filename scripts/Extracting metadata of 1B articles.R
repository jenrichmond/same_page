# Extracting metadata of 1B articles

library(tidyverse)
library(rcrossref)
library(usethis)
library(tidyverse)
library(listviewer)
library(here)
library(fuzzyjoin)
library(stringr)
library(dplyr)

# read in csv containing article DOIs  
dois <- read_csv(here("data_files", "1B_article_dois.csv"))

# pull just the DOIs into a list
dois_list <- dois$DOI

# pass the dois_list list to cr_works()
dois_works <- cr_works(dois = dois_list) %>% 
  pluck("data")

# print the relevant data into a data frame AND rename the variables
meta_done <- dois_works %>% 
  select(doi, container.title, published.print, title, author, volume, issue, page) %>%
  separate(published.print, c("Year", "Month"), sep = "-") %>%
  rename(DOI = doi, Journal = container.title, Title = title, Authors = author, Volume = volume, Issue = issue, Page_Number = page)

# let's add the Article Ids back into the dataset 

# first we need to remove the "https://doi.org/" section from the DOIs in the dois dataframe so the DOIs are in the same format
dois_clean <- dois %>%
  mutate_at("DOI", str_replace, "https://doi.org/", "")

# and now we can merge the original dataset (containing only DOIs) and the new one
meta_ids <- merge(dois_clean, meta_done, by="DOI")

# let's reorder the columns 
meta_ids <- meta_ids %>% 
  janitor::clean_names() %>%
  select(doi, article_id, authors, year, month, title, journal, volume, issue, page_number)

# let's convert the month column into words instead of numbers

data <- data %>% mutate(gender = 
                          factor(gender, 
                                 levels = c(1, 2),  
                                 labels = c("female", "male")))


meta_ids <- meta_ids %>%
  mutate(month = factor(month, 
                        levels = c("01", 
                                   "02", 
                                   "03", 
                                   "04", 
                                   "05", 
                                   "06", 
                                   "07", 
                                   "08", 
                                   "09", 
                                   "10", 
                                   "11", 
                                   "12"), 
         labels = c("Jan", 
                    "Feb",
                    "Mar", 
                    "Apr", 
                    "May", 
                    "Jun",
                    "Jul", 
                    "Aug", 
                    "Sep", 
                    "Oct", 
                    "Nov", 
                    "Dec")))


# Code to combine all authors of an article in one cell

# let's unnest the authors names
authors <- meta_ids %>%
  tidyr::unnest(authors, .drop = TRUE)

# let's combine first and last names AND drop the unnecessary columns
authors_clean <- authors %>%
  unite("authors", given:family) %>%
  select(doi, article_id, authors, year:page_number)

# now let's merge all the rows that have a common Article ID, so that all the authors names appear in the same cell


authors_string <- authors_clean %>%
  group_by(doi, article_id, year, month, title, journal, volume, issue, page_number) %>%
  summarise_all(funs(toString(na.omit(.))))


obs_test <- anti_join(meta_ids, authors_string, by = "article_id")


# Stuck here 
# https://stackoverflow.com/questions/51523082/how-to-combine-rows-with-the-same-identifier-r 

# write the dataframe as a csv. 
combined_authors %>%
  write_csv(here("data_files", "complete_1B_metadata.csv"))


