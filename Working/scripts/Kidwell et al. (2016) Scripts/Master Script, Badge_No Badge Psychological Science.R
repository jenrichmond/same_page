library(httr)

## THIS MASTER SCRIPT COMPILES THE RESULTS OF THE BADGE VS. NO BADGE ANALYSES WITHIN PSYCHOLOGICAL SCIENCE FOR THE FOLLOWING VARIABLES INTO ONE TABLE:
## ACTUAL AVAILABILITY (BADGE), ACTUAL AVAILABILITY (NO BADGE), CORRECTNESS (BADGE), CORRECTNESS (NO BADGE),
## USABILITY (BADGE), USABILITY (NO BADGE), COMPLETENESS (BADGE), AND COMPLETENESS (NO BADGE).

## NOTE: THIS R SCRIPT WILL AUTOMATICALLY IMPORT DATA FRAMES FROM EIGHT OTHER SCRIPTS ON THE OPEN SCIENCE FRAMEWORK (OSF).
## IF YOU WOULD PREFER TO MANUALLY DOWNLOAD THESE R SCRIPTS TO YOUR MACHINE INDIVIDUALLY AND IMPORT INTO R,
## PLEASE VISIT THE LINK TO EACH VARIABLE'S CORRESPONDING SCRIPT BELOW AND MODIFY THE SOURCE TO READ THE LOCATION OF THE FILE ON YOUR MACHINE.

## NOTE: THE INDIVIDUAL R SCRIPTS ALSO AUTOMATICALLY IMPORT THE DATA FILE NECESSARY TO RUN EACH INDIVIDUAL SCRIPT,
## "Master Dataset.csv," FROM THE OPEN SCIENCE FRAMEWORK.
## THE FOLLOWING TWO LINES ARE NOT NECESSARY TO RUN THIS SCRIPT, BUT THIS IS HOW THE DATA FILE IS AUTOMATICALLY IMPORTED 
## IN THE EIGHT INDIVIDUAL SCRIPTS:
## metadata <- GET('https://osf.io/a29bt/?action=download', write_disk('Master Dataset.csv', overwrite = TRUE))
## metadata <- as.data.frame(read.csv("Master Dataset.csv", header=T, sep=","))

## IF YOU WOULD PREFER TO MANUALLY DOWNLOAD THE DATA FILE TO YOUR MACHINE AND IMPORT INTO R,
## DOWNLOAD "Master Dataset.csv" FROM THE OPEN SCIENCE FRAMEWORK: https://osf.io/a29bt/

## Downloads R script "ActualAvailability_PS_Badge.R": https://osf.io/rznfw/
GET('https://osf.io/rznfw/?action=download', write_disk('ActualAvailability_PS_Badge.R', overwrite = TRUE))
source("ActualAvailability_PS_Badge.R")
rm(list=setdiff(ls(), c("ActualAvailability.MaterialsBadge", "ActualAvailability.DataBadge")))

## Downloads R script "ActualAvailability_PS_NoBadge.R": https://osf.io/a5rpb/
GET('https://osf.io/a5rpb/?action=download', write_disk('ActualAvailability_PS_NoBadge.R', overwrite = TRUE))
source("ActualAvailability_PS_NoBadge.R")
rm(list=setdiff(ls(), c("ActualAvailability.MaterialsBadge", "ActualAvailability.DataBadge", 
                        "ActualAvailability.NoMaterialsBadge", "ActualAvailability.NoDataBadge")))

## Downloads R script "Completeness_PS_Badge.R": https://osf.io/m6erk/
GET('https://osf.io/m6erk/?action=download', write_disk('Completeness_PS_Badge.R', overwrite = TRUE))
source("Completeness_PS_Badge.R")
rm(list=setdiff(ls(), c("ActualAvailability.MaterialsBadge", "ActualAvailability.DataBadge", 
                        "ActualAvailability.NoMaterialsBadge", "ActualAvailability.NoDataBadge",
                        "Completeness.MaterialsBadge", "Completeness.DataBadge")))

## Downloads R script "Completeness_PS_NoBadge.R": https://osf.io/t5unb/
GET('https://osf.io/t5unb/?action=download', write_disk('Completeness_PS_NoBadge.R', overwrite = TRUE))
source("Completeness_PS_NoBadge.R")
rm(list=setdiff(ls(), c("ActualAvailability.MaterialsBadge", "ActualAvailability.DataBadge", 
                        "ActualAvailability.NoMaterialsBadge", "ActualAvailability.NoDataBadge",
                        "Completeness.MaterialsBadge", "Completeness.DataBadge", 
                        "Completeness.NoMaterialsBadge", "Completeness.NoDataBadge")))

## Downloads R script "Correctness_PS_Badge.R": https://osf.io/wzyvx/
GET('https://osf.io/wzyvx/?action=download', write_disk('Correctness_PS_Badge.R', overwrite = TRUE))
source("Correctness_PS_Badge.R")
rm(list=setdiff(ls(), c("ActualAvailability.MaterialsBadge", "ActualAvailability.DataBadge", 
                        "ActualAvailability.NoMaterialsBadge", "ActualAvailability.NoDataBadge",
                        "Completeness.MaterialsBadge", "Completeness.DataBadge", 
                        "Completeness.NoMaterialsBadge", "Completeness.NoDataBadge",
                        "Correctness.MaterialsBadge", "Correctness.DataBadge")))

## Downloads R script "Correctness_PS_NoBadge.R": https://osf.io/2j3xr/
GET('https://osf.io/2j3xr/?action=download', write_disk('Correctness_PS_NoBadge.R', overwrite = TRUE))
source("Correctness_PS_NoBadge.R")
rm(list=setdiff(ls(), c("ActualAvailability.MaterialsBadge", "ActualAvailability.DataBadge", 
                        "ActualAvailability.NoMaterialsBadge", "ActualAvailability.NoDataBadge",
                        "Completeness.MaterialsBadge", "Completeness.DataBadge", 
                        "Completeness.NoMaterialsBadge", "Completeness.NoDataBadge",
                        "Correctness.MaterialsBadge", "Correctness.DataBadge",
                        "Correctness.NoMaterialsBadge", "Correctness.NoDataBadge")))

## Downloads R script "Usability_PS_Badge.R": https://osf.io/4aqj8/
GET('https://osf.io/4aqj8/?action=download', write_disk('Usability_PS_Badge.R', overwrite = TRUE))
source("Usability_PS_Badge.R")
rm(list=setdiff(ls(), c("ActualAvailability.MaterialsBadge", "ActualAvailability.DataBadge", 
                        "ActualAvailability.NoMaterialsBadge", "ActualAvailability.NoDataBadge",
                        "Completeness.MaterialsBadge", "Completeness.DataBadge", 
                        "Completeness.NoMaterialsBadge", "Completeness.NoDataBadge",
                        "Correctness.MaterialsBadge", "Correctness.DataBadge",
                        "Correctness.NoMaterialsBadge", "Correctness.NoDataBadge",
                        "Usability.MaterialsBadge", "Usability.DataBadge")))

## Downloads R script "Usability_PS_NoBadge.R": https://osf.io/4j53c/
GET('https://osf.io/4j53c/?action=download', write_disk('Usability_PS_NoBadge.R', overwrite = TRUE))
source("Usability_PS_NoBadge.R")
rm(list=setdiff(ls(), c("ActualAvailability.MaterialsBadge", "ActualAvailability.DataBadge", 
                        "ActualAvailability.NoMaterialsBadge", "ActualAvailability.NoDataBadge",
                        "Completeness.MaterialsBadge", "Completeness.DataBadge", 
                        "Completeness.NoMaterialsBadge", "Completeness.NoDataBadge",
                        "Correctness.MaterialsBadge", "Correctness.DataBadge",
                        "Correctness.NoMaterialsBadge", "Correctness.NoDataBadge",
                        "Usability.MaterialsBadge", "Usability.DataBadge",
                        "Usability.NoMaterialsBadge", "Usability.NoDataBadge")))

############ RESULTS BY YEAR, DATA: PSYCHOLOGICAL SCIENCE, BADGE VS. NO BADGE ##############

colnames(ActualAvailability.DataBadge) <- paste("Data Badge, Actual Availability: ",
                                           colnames(ActualAvailability.DataBadge))

colnames(Correctness.DataBadge) <- paste("Data Badge, Correctness: ", colnames(Correctness.DataBadge))

colnames(Usability.DataBadge) <- paste("Data Badge, Usability: ", colnames(Usability.DataBadge))

colnames(Completeness.DataBadge) <- paste("Data Badge, Completeness: ", colnames(Completeness.DataBadge))

colnames(ActualAvailability.NoDataBadge) <- paste("No Data Badge, Actual Availability: ",
                                                colnames(ActualAvailability.NoDataBadge))

colnames(Correctness.NoDataBadge) <- paste("No Data Badge, Correctness: ", colnames(Correctness.NoDataBadge))

colnames(Usability.NoDataBadge) <- paste("No Data Badge, Usability: ", colnames(Usability.NoDataBadge))

colnames(Completeness.NoDataBadge) <- paste("No Data Badge, Completeness: ", colnames(Completeness.NoDataBadge))

Master_Data_BadgevsNoBadge <- cbind(ActualAvailability.DataBadge,
                          Correctness.DataBadge[,2:5],
                          Usability.DataBadge[,2:5],
                          Completeness.DataBadge[,2:6],
                          ActualAvailability.NoDataBadge[,2:5],
                          Correctness.NoDataBadge[,2:5],
                          Usability.NoDataBadge[,2:5],
                          Completeness.NoDataBadge[,2:6])

colnames(Master_Data_BadgevsNoBadge)[1] <- "Year"

## Open dataframe "Master_Data_BadgevsNoBadge" for final output
## No data for CPS in 2012 is expected

############ RESULTS BY YEAR, MATERIALS: PSYCHOLOGICAL SCIENCE, BADGE VS. NO BADGE ##############

colnames(ActualAvailability.MaterialsBadge) <- paste("Materials Badge, Actual Availability: ",
                                                colnames(ActualAvailability.MaterialsBadge))

colnames(Correctness.MaterialsBadge) <- paste("Materials Badge, Correctness: ", colnames(Correctness.MaterialsBadge))

colnames(Usability.MaterialsBadge) <- paste("Materials Badge, Usability: ", colnames(Usability.MaterialsBadge))

colnames(Completeness.MaterialsBadge) <- paste("Materials Badge, Completeness: ", colnames(Completeness.MaterialsBadge))

colnames(ActualAvailability.NoMaterialsBadge) <- paste("No Materials Badge, Actual Availability: ",
                                                  colnames(ActualAvailability.NoMaterialsBadge))

colnames(Correctness.NoMaterialsBadge) <- paste("No Materials Badge, Correctness: ", colnames(Correctness.NoMaterialsBadge))

colnames(Usability.NoMaterialsBadge) <- paste("No Materials Badge, Usability: ", colnames(Usability.NoMaterialsBadge))

colnames(Completeness.NoMaterialsBadge) <- paste("No Materials Badge, Completeness: ", colnames(Completeness.NoMaterialsBadge))

Master_Materials_BadgevsNoBadge <- cbind(ActualAvailability.MaterialsBadge,
                                    Correctness.MaterialsBadge[,2:5],
                                    Usability.MaterialsBadge[,2:5],
                                    Completeness.MaterialsBadge[,2:6],
                                    ActualAvailability.NoMaterialsBadge[,2:5],
                                    Correctness.NoMaterialsBadge[,2:5],
                                    Usability.NoMaterialsBadge[,2:5],
                                    Completeness.NoMaterialsBadge[,2:6])

colnames(Master_Materials_BadgevsNoBadge)[1] <- "Year"

## Open dataframe "Master_Materials_BadgevsNoBadge" for final output
## No data for CPS in 2012 is expected