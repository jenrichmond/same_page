library(httr)

## THIS MASTER SCRIPT COMPILES THE RESULTS OF THE FOLLOWING MAIN ANALYSES VARIABLES INTO ONE TABLE:
## AVAILABILITY STATEMENTS (BY YEAR), ACTUAL AVAILABILITY, CORRECTNESS, USABILITY, COMPLETENESS, AND REPORTED LOCATION.

## NOTE: THIS R SCRIPT WILL AUTOMATICALLY IMPORT DATA FRAMES FROM SIX OTHER SCRIPTS ON THE OPEN SCIENCE FRAMEWORK (OSF).
## IF YOU WOULD PREFER TO MANUALLY DOWNLOAD THESE R SCRIPTS TO YOUR MACHINE INDIVIDUALLY AND IMPORT INTO R,
## PLEASE VISIT THE LINK TO EACH VARIABLE'S CORRESPONDING SCRIPT BELOW AND MODIFY THE SOURCE TO READ THE LOCATION OF THE FILE ON YOUR MACHINE.

## NOTE: THE INDIVIDUAL R SCRIPTS ALSO AUTOMATICALLY IMPORT THE DATA FILE NECESSARY TO RUN EACH INDIVIDUAL SCRIPT,
## "Master Dataset.csv," FROM THE OPEN SCIENCE FRAMEWORK.
## THE FOLLOWING TWO LINES ARE NOT NECESSARY TO RUN THIS SCRIPT, BUT THIS IS HOW THE DATA FILE IS AUTOMATICALLY IMPORTED 
## IN THE SIX INDIVIDUAL SCRIPTS:
## metadata <- GET('https://osf.io/a29bt/?action=download', write_disk('Master Dataset.csv', overwrite = TRUE))
## metadata <- as.data.frame(read.csv("Master Dataset.csv", header=T, sep=","))

## IF YOU WOULD PREFER TO MANUALLY DOWNLOAD THE DATA FILE TO YOUR MACHINE AND IMPORT INTO R,
## DOWNLOAD "Master Dataset.csv" FROM THE OPEN SCIENCE FRAMEWORK: https://osf.io/a29bt/

## Downloads R script "Availability Statements By Year, All Journals.R": https://osf.io/snzhy/
GET('https://osf.io/snzhy/?action=download', write_disk('Availability Statements By Year, All Journals.R', overwrite = TRUE))
source("Availability Statements By Year, All Journals.R")
rm(list=setdiff(ls(), c("AvailabilityStatement.Materials.Year", "AvailabilityStatement.Data.Year")))

## Downloads R script "Actual Availability, All Journals.R": https://osf.io/hj8pf/
GET('https://osf.io/hj8pf/?action=download', write_disk('Actual Availability, All Journals.R', overwrite = TRUE))
source("Actual Availability, All Journals.R")
rm(list=setdiff(ls(), c("AvailabilityStatement.Materials.Year", "AvailabilityStatement.Data.Year", 
                        "ActualAvailability.Materials", "ActualAvailability.Data")))

## Downloads R script "Correctness, All Journals.R": https://osf.io/jptcg/
GET('https://osf.io/jptcg/?action=download', write_disk('Correctness, All Journals.R', overwrite = TRUE))
source("Correctness, All Journals.R")
rm(list=setdiff(ls(), c("AvailabilityStatement.Materials.Year", "AvailabilityStatement.Data.Year", 
                        "ActualAvailability.Materials", "ActualAvailability.Data",
                        "Correctness.Materials", "Correctness.Data")))

## Downloads R script "Usability, All Journals.R": https://osf.io/wz592/
GET('https://osf.io/wz592/?action=download', write_disk('Usability, All Journals.R', overwrite = TRUE))
source("Usability, All Journals.R")
rm(list=setdiff(ls(), c("AvailabilityStatement.Materials.Year", "AvailabilityStatement.Data.Year", 
                        "ActualAvailability.Materials", "ActualAvailability.Data",
                        "Correctness.Materials", "Correctness.Data", 
                        "Usability.Materials", "Usability.Data")))

## Downloads R script "Completeness, All Journals.R": https://osf.io/sm3x2/
GET('https://osf.io/sm3x2/?action=download', write_disk('Completeness, All Journals.R', overwrite = TRUE))
source("Completeness, All Journals.R")
rm(list=setdiff(ls(), c("AvailabilityStatement.Materials.Year", "AvailabilityStatement.Data.Year", 
                          "ActualAvailability.Materials", "ActualAvailability.Data",
                          "Correctness.Materials", "Correctness.Data",
                          "Usability.Materials", "Usability.Data",
                          "Completeness.Materials", "Completeness.Data")))

## Downloads R script "Reported Location, All Journals.R": https://osf.io/t3e8v/
GET('https://osf.io/t3e8v/?action=download', write_disk('Reported Location, All Journals.R', overwrite = TRUE))
source("Reported Location, All Journals.R")
rm(list=setdiff(ls(), c("AvailabilityStatement.Materials.Year", "AvailabilityStatement.Data.Year", 
                        "ActualAvailability.Materials", "ActualAvailability.Data",
                        "Correctness.Materials", "Correctness.Data",
                        "Usability.Materials", "Usability.Data",
                        "Completeness.Materials", "Completeness.Data",
                        "ReportedLocation.Materials", "ReportedLocation.Data")))

############ RESULTS BY YEAR: ALL JOURNALS, DATA ##############

colnames(AvailabilityStatement.Data.Year) <- paste("Availability Statement: ",
                                                   colnames(AvailabilityStatement.Data.Year))

colnames(ActualAvailability.Data) <- paste("Actual Availability: ",
                                           colnames(ActualAvailability.Data))

colnames(Correctness.Data) <- paste("Correctness: ", colnames(Correctness.Data))

colnames(Usability.Data) <- paste("Usability: ", colnames(Usability.Data))

colnames(Completeness.Data) <- paste("Completeness: ", colnames(Completeness.Data))

colnames(ReportedLocation.Data) <- paste("Reported Location: ", colnames(ReportedLocation.Data))

Master_data_year <- cbind(AvailabilityStatement.Data.Year,
                          ActualAvailability.Data[,3:6],
                          Correctness.Data[,3:6],
                          Usability.Data[,3:6],
                          Completeness.Data[,3:7],
                          ReportedLocation.Data[,3:8])

colnames(Master_data_year)[1:2] <- c("Year","Journal")

## Open dataframe "Master_data_year" for final output
## No data for CPS in 2012 is expected

############ RESULTS BY YEAR: MATERIALS ##############

colnames(AvailabilityStatement.Materials.Year) <- paste("Availability Statement: ",
                                                   colnames(AvailabilityStatement.Materials.Year))

colnames(ActualAvailability.Materials) <- paste("Actual Availability: ",
                                           colnames(ActualAvailability.Materials))

colnames(Correctness.Materials) <- paste("Correctness: ", colnames(Correctness.Materials))

colnames(Usability.Materials) <- paste("Usability: ", colnames(Usability.Materials))

colnames(Completeness.Materials) <- paste("Completeness: ", colnames(Completeness.Materials))

colnames(ReportedLocation.Materials) <- paste("Reported Location: ", colnames(ReportedLocation.Materials))

Master_materials_year <- cbind(AvailabilityStatement.Materials.Year,
                          ActualAvailability.Materials[,3:6],
                          Correctness.Materials[,3:6],
                          Usability.Materials[,3:6],
                          Completeness.Materials[,3:7],
                          ReportedLocation.Materials[,3:8])

colnames(Master_materials_year)[1:2] <- c("Year","Journal")

## Open dataframe "Master_materials_year" for final output
## No data for CPS in 2012 is expected