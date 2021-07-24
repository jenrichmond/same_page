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
  relocate("Article ID", .before = DOI) %>%
  relocate ("Authors", .after = `Article ID`) %>%
  relocate("Year", .after = Authors) %>%
  relocate("Month", .after = Year) %>%
  relocate("Title", .after = Month) %>%
  relocate("Journal", .after = DOI) %>%
  relocate("Volume", .after = Journal) %>%
  relocate("Issue", .after = Volume) %>%
  relocate("Page_Number", .after = Issue)

# let's convert the month column into words instead of numbers
meta_ids <- meta_ids %>%
  mutate(Month = case_when(`01` == "Jan", `02` == "Feb", `03` == "Mar", `04` == "Apr", `05` == "May", `06` == "Jun", `07` == "Jul", `08` == "Aug", `09` == "Sep", `10` == "Oct", `11` == "Nov", `12` == "Dec"))    
# there has to be an easier way to do this?

# Code to combine all authors of an article in one cell

# let's unnest the authors names
authors <- meta_ids %>%
  tidyr::unnest(Authors, .drop = TRUE)

# let's combine first and last names AND drop the unnecessary columns
authors_clean <- authors %>%
  unite("Authors", given:family) %>%
  select(`Article ID`, Authors, Year:Page_Number)

# now let's merge all the rows that have a common Article ID, so that all the authors names appear in the same cell
combined_authors <- authors_clean %>%
  group_by("Article ID") %>%
  summarise_all(funs(toString(na.omit(.))))

combined_authors_2 <- authors_clean %>%
  group_by("Article ID") %>%
  summarise_all(funs(toString("Article ID")))

combined_authors_1 <- authors_clean %>%
  group_by("DOI") %>%
  summarise_all(funs(paste(., collapse = ".")))
  
combined_authors_3 <- authors_clean %>%
  group_by("DOI") %>%
  summarise_all(paste, collapse = '')

combined_authors_4 <- authors_clean %>%
  group_by("Article ID") %>%
  summarise_all(funs(paste(., collapse = ",")))

combined_authors_5 <- authors_clean %>%
  group_by("Article ID") %>%
  summarise_all(funs(paste(., sep = '', collapse = NULL)))

# Stuck here 
# https://stackoverflow.com/questions/51523082/how-to-combine-rows-with-the-same-identifier-r 

# write the dataframe as a csv. 
combined_authors %>%
  write_csv(here("data_files", "complete_1B_metadata.csv"))


