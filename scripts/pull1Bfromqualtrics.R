# Load packages
library(qualtRics)
library(tidyverse)
library(janitor)
library(ggplot2)
library(grid)
library(cowplot)
library(httr)
library(extrafont)
library(here)

# Import 1B data from qualtrics 

qualtrics_api_credentials(api_key = "RZNCuPM7MvUJmhi7wcSLN7NHx0WCVeIU6H7XiSeE", 
                          base_url = "unsw.syd1.qualtrics.com",
                          install = TRUE, 
                          overwrite = TRUE)

# Replace {  } with the datacenter ID from Account Setting = syd1
# https://{datacenterid}.qualtrics.com/API/V3/pathToRequest
# https://syd1.qualtrics.com/API/V3/pathToRequest

readRenviron("~/.Renviron")  



# Read in the questions and survey data from the 1B survey
# 1B ID = SV_86Um9YiXUcH6eGy

# pull 1B questions
questions <- survey_questions(surveyID = "SV_86Um9YiXUcH6eGy")

# write questions to csv

questions %>%
  write_csv(here::here("data_files", "questions1B.csv"))

# pull 1B data
data1B  <- fetch_survey(surveyID = "SV_86Um9YiXUcH6eGy", verbose = TRUE,force_request = TRUE) %>%
  clean_names()

# write 1B data to csv

data1B %>% write_csv(here::here("data_files", "data1B.csv"))
