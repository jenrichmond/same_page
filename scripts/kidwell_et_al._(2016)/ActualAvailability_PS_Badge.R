library(tidyr)
library(httr)

## THIS SCRIPT BREAKS DOWN THE ACTUAL AVAILABILITY OF DATA AND MATERIALS IN PSYCHOLOGICAL SCIENCE THAT WERE REPORTEDLY AVAILABLE AT OPEN ACCESS LOCATIONS AND AWARDED A BADGE.

## NOTE: THIS SCRIPT WILL AUTOMATICALLY IMPORT THE DATA FILE NECESSARY TO RUN THIS SCRIPT, "Master Dataset.csv," FROM THE OPEN SCIENCE FRAMEWORK (OSF).

## IF YOU WOULD PREFER TO MANUALLY DOWNLOAD THE DATA FILE TO YOUR MACHINE AND IMPORT INTO R, PLEASE:
## 1. DOWNLOAD "Master Dataset.csv" FROM THE OPEN SCIENCE FRAMEWORK: https://osf.io/a29bt/
## 2. DO NOT RUN LINE 14
## 3. MODIFY LINE 15 TO READ THE LOCATION OF THE FILE ON YOUR MACHINE.

# Import tables from OSF file "Master Dataset.csv"
metadata <- GET('https://osf.io/a29bt/?action=download', write_disk('Master Dataset.csv', overwrite = TRUE))
metadata <- as.data.frame(read.csv("Master Dataset.csv", header=T, sep=","))

# Remove any article IDs that are not from Psychological Science
PS_metadata <- subset(metadata, grepl(" PS", metadata$Article.ID.number))

# Remove any articles that are not empirical in nature (commentaries, corrigendum, corrections, editorials, etc.)
PS_empirical_metadata <- subset(PS_metadata, Number.of.experiments > 0)

########### ACTUAL AVAILABILITY: DATA BADGE ##############

# for loop to see within each year how many article received data badge and if they were found at their location
PS_openaccess_data <- subset(PS_empirical_metadata, Data.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

# for loop to see within each year how many article received data badge and if they were found at their location
dataavailability.results <- data.frame() #empty place to store results

for (year in (c(2012, 2013, 2014, 2015))){
  print(year) #make sure loop is working
  papers <- subset(PS_openaccess_data, grepl(year, PS_openaccess_data$Article.ID.number)) #subsetting papers by year
  papers <- subset(papers, papers[7] == "Yes")
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.data.~Are.the.data.located.at.the.working.page.,
                    data=papers, FUN=table) #of articles that had a data availability statement, are the data located where indicated. FUN is for summary statistics
  result <- data.frame(year, Are.the.data.located.at.the.working.page.=intm$Are.the.data.located.at.the.working.page.  ,
                       intm$Did.the.article.receive.a.badge.for.open.data.) #build data frame with year and title, =intm$ recall and produce output
  dataavailability.results <- rbind(dataavailability.results, result) #combine results together
} #end of loop, signals to go back to next year
dataavailability.results
sum(dataavailability.results$No, dataavailability.results$Yes) #double check to make sure all articles are included

#reorganize the R output
Dunorganized <- dataavailability.results
Total <- rowSums(Dunorganized[,c("Yes", "No")])
withtotal <- cbind(Dunorganized, Total)
keeps <- c("year", "Are.the.data.located.at.the.working.page.", "Yes") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]
ActualAvailability.DataBadge <- spread(keepcolumns, Are.the.data.located.at.the.working.page., Yes)
ActualAvailability.DataBadge[is.na(ActualAvailability.DataBadge)] <- 0

colnames(ActualAvailability.DataBadge)[2] <- "N/A" # Manual curation
ActualAvailability.DataBadge[,"No"] <- 0 # Manual curation, adding known zeroes

# At this point you should have 6 columns
colnames(ActualAvailability.DataBadge) #  puts out the order so you can rearrange
ActualAvailability.DataBadge <- ActualAvailability.DataBadge[c(1,4,5,3,2)] # Rearrange columns so they match the order you want
Before_badges <- colSums(ActualAvailability.DataBadge[1:2, c(2,3,4,5)])
Before_badges <- c("2012-2013", Before_badges)
After_badges <- colSums(ActualAvailability.DataBadge[3:4, c(2,3,4,5)])
After_badges <- c("2014-2015", After_badges)
ActualAvailability.DataBadge <- rbind(ActualAvailability.DataBadge, Before_badges, After_badges)

## Open dataframe "ActualAvailability.DataBadge" for final output

########## ACTUAL AVAILABILITY: MATERIALS BADGE ##############

# for loop to see within each year how many article received data badge and if they were found at their location
PS_openaccess_materials <- subset(PS_empirical_metadata, Materials.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

# for loop to see within each year how many article received materials badge and if they were found at their location
materialavailability.results <- data.frame()

for (year in (c(2012, 2013, 2014, 2015))){
  print(year) #make sure loop is working
  papers <- subset(PS_openaccess_materials, grepl(year, PS_openaccess_materials$Article.ID.number)) #subsetting papers by year
  papers <- subset(papers, papers[25] == "Yes")
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.materials.~Are.the.materials.located.at.the.working.page.,
                    data=papers, FUN=table) #of articles that had a materials availability statement, are the materials located where indicated. FUN is for summary statistics
  result <- data.frame(year, Are.the.materials.located.at.the.working.page.=intm$Are.the.materials.located.at.the.working.page.  ,
                       intm$Did.the.article.receive.a.badge.for.open.materials.) #build data frame with year and title, =intm$ recall and produce output
  materialavailability.results <- rbind(materialavailability.results, result) #combine results together
} #end of loop, signals to go back to next year
materialavailability.results
sum(materialavailability.results$No, materialavailability.results$Yes) #double check to make sure all articles are included

#reorganize the R output
Munorganized <- materialavailability.results
Total <- rowSums(Munorganized[,c("Yes", "No")])
withtotal <- cbind(Munorganized, Total)
keeps <- c("year", "Are.the.materials.located.at.the.working.page.", "Yes") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]
ActualAvailability.MaterialsBadge <- spread(keepcolumns, Are.the.materials.located.at.the.working.page., Yes)
ActualAvailability.MaterialsBadge[is.na(ActualAvailability.MaterialsBadge)] <- 0

colnames(ActualAvailability.MaterialsBadge)[2] <- "N/A" # Manual curation

# At this point you should have 5 columns
colnames(ActualAvailability.MaterialsBadge) #  puts out the order so you can rearrange
ActualAvailability.MaterialsBadge <- ActualAvailability.MaterialsBadge[c(1,5,3,4,2)] # Rearrange columns so they match the order you want
Before_badges <- colSums(ActualAvailability.MaterialsBadge[1:2, c(2,3,4,5)])
Before_badges <- c("2012-2013", Before_badges)
After_badges <- colSums(ActualAvailability.MaterialsBadge[3:4, c(2,3,4,5)])
After_badges <- c("2014-2015", After_badges)
ActualAvailability.MaterialsBadge <- rbind(ActualAvailability.MaterialsBadge, Before_badges, After_badges)

## Open dataframe "ActualAvailability.MaterialsBadge" for final output