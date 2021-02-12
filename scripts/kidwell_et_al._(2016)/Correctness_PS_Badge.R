library(tidyr)
library(httr)

## THIS SCRIPT BREAKS DOWN THE CORRECTNESS OF DATA AND MATERIALS IN PSYCHOLOGICAL SCIENCE THAT WERE REPORTEDLY AVAILABLE AT OPEN ACCESS LOCATIONS AND AWARDED A BADGE.

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

########### CORRECTNESS: DATA BADGE ##############

# for loop to see within each year how many article received data badge and if they were found at their location
PS_openaccess_data <- subset(PS_empirical_metadata, Data.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

# for loop to see within each year how many article received data badge and if they were found at their location
datacorrectness.results <- data.frame() #empty place to store results

for (year in (c(2012, 2013, 2014, 2015))){
  print(year) #make sure loop is working
  papers <- subset(PS_openaccess_data, grepl(year, PS_openaccess_data$Article.ID.number)) #subsetting papers by year
  papers <- subset(papers, papers[7] == "Yes")
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.data.~Does.the.data.correspond.to.what.is.reported.in.the.article.,
                    data=papers, FUN=table) #of articles that had a data availability statement, does the content of the data correspond to what is reported. FUN is for summary statistics
  result <- data.frame(year, Does.the.data.correspond.to.what.is.reported.in.the.article.=intm$Does.the.data.correspond.to.what.is.reported.in.the.article.  ,
                       intm$Did.the.article.receive.a.badge.for.open.data.) #build data frame with year and title, =intm$ recall and produce output
  datacorrectness.results <- rbind(datacorrectness.results, result) #combine results together
} #end of loop, signals to go back to next year
datacorrectness.results
sum(datacorrectness.results$No, datacorrectness.results$Yes) #double check to make sure all articles are included

#reorganize the R output
Dunorganized <- datacorrectness.results
Total <- rowSums(Dunorganized[,c("Yes", "No")])
withtotal <- cbind(Dunorganized, Total)
keeps <- c("year", "Does.the.data.correspond.to.what.is.reported.in.the.article.", "Yes") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]
Correctness.DataBadge <- spread(keepcolumns, Does.the.data.correspond.to.what.is.reported.in.the.article., Yes)
Correctness.DataBadge[is.na(Correctness.DataBadge)] <- 0

colnames(Correctness.DataBadge)[2] <- "N/A" # Manual curation
Correctness.DataBadge[, "No"] <- 0

# At this point you should have 5 columns
colnames(Correctness.DataBadge) #  puts out the order so you can rearrange
Correctness.DataBadge <- Correctness.DataBadge[c(1,4,5,3,2)] # Rearrange columns so they match the order you want
Before_badges <- colSums(Correctness.DataBadge[1:2, c(2,3,4,5)])
Before_badges <- c("2012-2013", Before_badges)
After_badges <- colSums(Correctness.DataBadge[3:4, c(2,3,4,5)])
After_badges <- c("2014-2015", After_badges)
Correctness.DataBadge <- rbind(Correctness.DataBadge, Before_badges, After_badges)

## Open dataframe "Correctness.DataBadge" for final output

########## CORRECTNESS: MATERIALS BADGE ##############

# for loop to see within each year how many article received data badge and if they were found at their location
PS_openaccess_materials <- subset(PS_empirical_metadata, Materials.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

# for loop to see within each year how many article received materials badge and if they were found at their location
materialcorrectness.results <- data.frame()

for (year in (c(2012, 2013, 2014, 2015))){
  print(year) #make sure loop is working
  papers <- subset(PS_openaccess_materials, grepl(year, PS_openaccess_materials$Article.ID.number)) #subsetting papers by year
  papers <- subset(papers, papers[25] == "Yes")
  if (nrow(papers) > 0){
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.materials.~Do.the.materials.correspond.to.what.is.reported.in.the.article.,
                    data=papers, FUN=table) #of articles that had a materials availability statement, does the content of the materials correspond to what is reported. FUN is for summary statistics
  result <- data.frame(year, Do.the.materials.correspond.to.what.is.reported.in.the.article.=intm$Do.the.materials.correspond.to.what.is.reported.in.the.article.  ,
                       intm$Did.the.article.receive.a.badge.for.open.materials.) #build data frame with year and title, =intm$ recall and produce output
  materialcorrectness.results <- rbind(materialcorrectness.results, result) #combine results together
  }
} #end of loop, signals to go back to next year
materialcorrectness.results
sum(materialcorrectness.results$No, materialcorrectness.results$Yes) #double check to make sure all articles are included

#reorganize the R output
Munorganized <- materialcorrectness.results
Total <- rowSums(Munorganized[,c("Yes", "No")])
withtotal <- cbind(Munorganized, Total)
keeps <- c("year", "Do.the.materials.correspond.to.what.is.reported.in.the.article.", "Yes") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]
Correctness.MaterialsBadge <- spread(keepcolumns, Do.the.materials.correspond.to.what.is.reported.in.the.article., Yes)
Correctness.MaterialsBadge[is.na(Correctness.MaterialsBadge)] <- 0

colnames(Correctness.MaterialsBadge)[2] <- "N/A" # Manual curation
Correctness.MaterialsBadge[, "No"] <- 0

# At this point you should have 5 columns
colnames(Correctness.MaterialsBadge) #  puts out the order so you can rearrange
Correctness.MaterialsBadge <- Correctness.MaterialsBadge[c(1,4,5,3,2)] # Rearrange columns so they match the order you want
Before_badges <- colSums(Correctness.MaterialsBadge[1:2, c(2,3,4,5)])
Before_badges <- c("2012-2013", Before_badges)
After_badges <- colSums(Correctness.MaterialsBadge[3:4, c(2,3,4,5)])
After_badges <- c("2014-2015", After_badges)
Correctness.MaterialsBadge <- rbind(Correctness.MaterialsBadge, Before_badges, After_badges)

## Open dataframe "Correctness.MaterialsBadge" for final output