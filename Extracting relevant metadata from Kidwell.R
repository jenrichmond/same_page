# Extracting relevant metadata from Kidwell

# read in csv containing article DOIs  
kidwell_metadata <- read_csv(here("data_files", "Kidwell Bibliographic Metadata.csv"))

# filter for only PS articles published between 2014 and 2015
clean_metadata <- kidwell_metadata %>%
  filter(Year %in% c("2014", "2015")) %>%
  filter(Journal == "PS") %>%
  select(`Article ID`, Title, Authors, Year, Month, Journal, Volume, Issue, Pages, DOI) %>%
  mutate(Journal = case_when(Journal == "PS" ~ "Psychological Science")) 

# Separate article_id_number variable into ID and journal code
ID_metadata <- clean_metadata %>%
  separate(`Article ID`, into = c("Article Identification Number", "journal_code"), sep = "\\s", remove = TRUE)

final_metadata <- ID_metadata %>%
  select(`Article Identification Number`, Title, Authors, Year, Month, Journal, Volume, Issue, `Page Numbers` = Pages, DOI)

# write the dataframe as a csv. 
final_metadata %>%
  write_csv(here("data_files", "Study 1A Bibliographic Metadata.csv"))