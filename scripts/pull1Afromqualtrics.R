

# Import 1A data from qualtrics 

qualtrics_api_credentials(api_key = "RZNCuPM7MvUJmhi7wcSLN7NHx0WCVeIU6H7XiSeE", 
                          base_url = "unsw.syd1.qualtrics.com",
                          install = TRUE, 
                          overwrite = TRUE)

# Replace {  } with the datacenter ID from Account Setting = syd1
# https://{datacenterid}.qualtrics.com/API/V3/pathToRequest
# https://syd1.qualtrics.com/API/V3/pathToRequest

readRenviron("~/.Renviron")  



# Read in the questions and survey data from the 1A survey
# 1A ID = SV_0GSSTcRvY9x0x4q

# pull 1A questions
questions <- survey_questions(surveyID = "SV_0GSSTcRvY9x0x4q")

# write questions to csv

questions %>%
  write_csv(here::here("data_files", "questions1A.csv"))

# pull 1A data
data1A  <- fetch_survey(surveyID = "SV_0GSSTcRvY9x0x4q", verbose = TRUE,force_request = TRUE) %>%
  clean_names()

# write 1A data to csv

data1A %>% write_csv(here::here("data_files", "data1A.csv"))
