library(tidyr)
library(httr)

## THIS SCRIPT BREAKS DOWN THE USABILITY OF DATA AND MATERIALS IN PSYCHOLOGICAL SCIENCE THAT WERE REPORTEDLY AVAILABLE AT OPEN ACCESS LOCATIONS AND AWARDED A BADGE.

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

########### USABILITY: DATA BADGE ##############

# for loop to see within each year how many article received data badge and if they were found at their location
PS_openaccess_data <- subset(PS_empirical_metadata, Data.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

# for loop to see within each year how many article received data badge and if they were understandable/usable
datausability.results <- data.frame() #empty place to store results

for (year in (c(2012, 2013, 2014, 2015))){
  print(year) #make sure loop is working
  papers <- subset(PS_openaccess_data, grepl(year, PS_openaccess_data$Article.ID.number)) #subsetting papers by year
  papers <- subset(papers, papers[7] == "Yes")
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.data.~Are.the.data.understandable.and.usable.after.brief.review.,
                    data=papers, FUN=table) #of articles that had a data availability statement, are the data complete. FUN is for summary statistics
  result <- data.frame(year, Are.the.data.understandable.and.usable.after.brief.review.=intm$Are.the.data.understandable.and.usable.after.brief.review.  ,
                       intm$Did.the.article.receive.a.badge.for.open.data.) #build data frame with year and title, =intm$ recall and produce output
  datausability.results <- rbind(datausability.results, result) #combine results together
} #end of loop, signals to go back to next year
datausability.results
sum(datausability.results$No, datausability.results$Yes) #double check to make sure all articles are included

#reorganize the R output
Dunorganized.Usability <- datausability.results
Total <- rowSums(Dunorganized.Usability[,c("Yes", "No")])
withtotal <- cbind(Dunorganized.Usability, Total)
keeps <- c("year", "Are.the.data.understandable.and.usable.after.brief.review.", "Yes") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]
Usability.DataBadge <- spread(keepcolumns, Are.the.data.understandable.and.usable.after.brief.review., Yes)
Usability.DataBadge[is.na(Usability.DataBadge)] <- 0

colnames(Usability.DataBadge)[2] <- "N/A" # Manual curation

# At this point you should have 5 columns
colnames(Usability.DataBadge) #  puts out the order so you can rearrange
Usability.DataBadge <- Usability.DataBadge[c(1,5,3,4,2)] # Rearrange columns so they match the order you want
Before_badges <- colSums(Usability.DataBadge[1:2, c(2,3,4,5)])
Before_badges <- c("2012-2013", Before_badges)
After_badges <- colSums(Usability.DataBadge[3:4, c(2,3,4,5)])
After_badges <- c("2014-2015", After_badges)
Usability.DataBadge <- rbind(Usability.DataBadge, Before_badges, After_badges)

#Open dataframe "Usability.DataBadge" for final output

########## USABILITY: MATERIALS BADGE ##############

# for loop to see within each year how many article received data badge and if they were found at their location
PS_openaccess_materials <- subset(PS_empirical_metadata, Materials.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

# for loop to see within each year how many article received materials badge and if they were understandable/usable
materialusability.results <- data.frame()

for (year in (c(2012, 2013, 2014, 2015))){
  print(year) #make sure loop is working
  papers <- subset(PS_openaccess_materials, grepl(year, PS_openaccess_materials$Article.ID.number)) #subsetting papers by year
  papers <- subset(papers, papers[25] == "Yes")
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.materials.~Are.the.materials.understandable.and.usable.after.brief.review.,
                    data=papers, FUN=table) #of articles that had a materials availability statement, are the materials located where indicated. FUN is for summary statistics
  result <- data.frame(year, Are.the.materials.understandable.and.usable.after.brief.review.=intm$Are.the.materials.understandable.and.usable.after.brief.review.  ,
                       intm$Did.the.article.receive.a.badge.for.open.materials.) #build data frame with year and title, =intm$ recall and produce output
  materialusability.results <- rbind(materialusability.results, result) #combine results together
} #end of loop, signals to go back to next year
materialusability.results
sum(materialusability.results$No, materialusability.results$Yes) #double check to make sure all articles are included

#reorganize the R output
Munorganized.Usability <- materialusability.results
Total <- rowSums(Munorganized.Usability[,c("Yes", "No")])
withtotal <- cbind(Munorganized.Usability, Total)
keeps <- c("year", "Are.the.materials.understandable.and.usable.after.brief.review.", "Yes") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]
Usability.MaterialsBadge <- spread(keepcolumns, Are.the.materials.understandable.and.usable.after.brief.review., Yes)
Usability.MaterialsBadge[is.na(Usability.MaterialsBadge)] <- 0

colnames(Usability.MaterialsBadge)[2] <- "N/A" # Manual curation

# At this point you should have 5 columns
colnames(Usability.MaterialsBadge) #  puts out the order so you can rearrange
Usability.MaterialsBadge <- Usability.MaterialsBadge[c(1,5,3,4,2)] # Rearrange columns so they match the order you want
Before_badges <- colSums(Usability.MaterialsBadge[1:2, c(2,3,4,5)])
Before_badges <- c("2012-2013", Before_badges)
After_badges <- colSums(Usability.MaterialsBadge[3:4, c(2,3,4,5)])
After_badges <- c("2014-2015", After_badges)
Usability.MaterialsBadge <- rbind(Usability.MaterialsBadge, Before_badges, After_badges)

#Open dataframe "Usability.MaterialsBadge" for final output