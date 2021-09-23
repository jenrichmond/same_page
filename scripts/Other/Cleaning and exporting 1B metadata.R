# Cleaning and exporting 1B metadata

metadata <- read_csv(here("data_files", "complete_1B_metadata.csv"))

# clean names
clean_metadata <- metadata %>%
  select(`Article Identification Number` = article_id, Title = title, Authors = authors, Year = year, Month = month, Journal = journal, Volume = volume, Issue = issue, `Page Numbers` = page_number, DOI = doi)

# write the dataframe as a csv. 
clean_metadata %>%
  write_csv(here("data_files", "Study 1B Bibliographic Metadata.csv"))