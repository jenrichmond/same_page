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

meta_ids <- meta_ids %>%
  mutate(month = factor(month, 
                        levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
         labels = c("Jan", "Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

# The authors of some articles (namely, corrigenda and errata) have not been correctly coded
  # instead of the authors names, NULL is used instead
  # for now, let's replace NULL with blank

NULL_authors <- meta_ids %>%
  filter(authors == "NULL") %>%
  mutate(authors = case_when("NULL" == authors ~ " "))

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

# let's combine the authors_string dataset with the NULL_authors dataset to get a complete dataset

complete <- rbind(authors_string, NULL_authors) %>%
  select(article_id, authors, year, month, title, doi, journal, volume, issue, page_number)

# write the dataframe as a csv. 
complete %>%
  write_csv(here("data_files", "complete_1B_metadata.csv"))


