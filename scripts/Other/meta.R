# rcrossref package https://ciakovx.github.io/rcrossref.html#Setting_up_rcrossref


library(tidyverse)
library(rcrossref)
library(usethis)
library(tidyverse)
library(listviewer)
library(here)

#using cr_works() to get data on articles, searching by DOI
#https://ciakovx.github.io/rcrossref.html#Using_cr_works()_to_get_data_on_articles

usethis::edit_r_environ()


# example from vignette------------
# Get metadata for a single article by DOI
jlsc_ku_oa <- cr_works(dois = "10.7710/2162-3309.1252") %>% 
                  purrr::pluck("data")

# print the data frame with select columns
jlsc_ku_oa %>% dplyr::select(title, doi, volume, issue, page, issued, url, publisher, 
                             reference.count, type, issn)


# psychsci example---------------

ps_test <- cr_works(dois = "https://doi.org/10.1177/0956797619847164") %>% 
  purrr::pluck("data") 

ps_test <- ps_test %>%
  select(doi, container.title, published.print, title, author, volume, issue, page) %>%
  separate(published.print, c("year", "month"), sep = "-")

# note the author field is a list, because there is normally more than 1 author
# there is an example in the vignette that illustrates how to unnest this field, 
# ... but it creates a row in the dataframe for each author, which may not be what we want 

# read in metadata sample, can use the DOI in the sample df to pull these details for lots of papers at once?  
meta <- read_csv(here("data_files", "sample_metadata_1B.csv"))

# pull just the DOIs into a list
my_dois <- meta$DOI

# pass the my_dois list to cr_works()

my_dois_works <- cr_works(dois = my_dois) %>% 
  pluck("data")

# print the data frame with select columns
meta_done <- my_dois_works %>% 
  select(doi, container.title, published.print, title, author, volume, issue, page) %>%
  separate(published.print, c("year", "month"), sep = "-")

# HMMMM this wont write to csv BECAUSE the author is a list  and csv is a "flat" file

meta_done %>%
  write_csv(here("data_files", "sample_metadata_pulled_1B.csv"))
