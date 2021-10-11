library(tidyr)
library(httr)

## THIS SCRIPT BREAKS DOWN THE ACTUAL AVAILABILITY OF DATA AND MATERIALS REPORTEDLY AVAILABLE AT OPEN ACCESS LOCATIONS BY YEAR AND JOURNAL.

## NOTE: THIS SCRIPT WILL AUTOMATICALLY IMPORT THE DATA FILE NECESSARY TO RUN THIS SCRIPT, "Master Dataset.csv," FROM THE OPEN SCIENCE FRAMEWORK (OSF).

## IF YOU WOULD PREFER TO MANUALLY DOWNLOAD THE DATA FILE TO YOUR MACHINE AND IMPORT INTO R, PLEASE:
## 1. DOWNLOAD "Master Dataset.csv" FROM THE OPEN SCIENCE FRAMEWORK: https://osf.io/a29bt/
## 2. DO NOT RUN LINE 14
## 3. MODIFY LINE 15 TO READ THE LOCATION OF THE FILE ON YOUR MACHINE.

# Import tables from OSF file "Master Dataset.csv"
metadata <- GET('https://osf.io/a29bt/?action=download', write_disk('Master Dataset.csv', overwrite = TRUE))
metadata <- as.data.frame(read.csv("Master Dataset.csv", header=T, sep=","))

# Remove any articles that are not empirical in nature (commentaries, corrigendum, corrections, editorials, etc.)
empirical_metadata <- subset(metadata, Number.of.experiments > 0)

# Assign journal names based on Article ID Number
empirical_metadata$Journals <- ifelse(grepl(" PS", empirical_metadata$Article.ID.number), "Psychological Science",
                               ifelse(grepl(" CPS", empirical_metadata$Article.ID.number), "Clinical Psychological Science",
                               ifelse(grepl(" DP", empirical_metadata$Article.ID.number), "Developmental Psychology",
                               ifelse(grepl(" JEPLMC", empirical_metadata$Article.ID.number), "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                               ifelse(grepl(" JPSP", empirical_metadata$Article.ID.number), "Journal of Personality and Social Psychology",
                                            NA  )))))

########################## ACTUAL AVAILABILITY: DATA ##############################

# for loop to see within each year how many article received data badge and if they were found at their location
openaccess_data <- subset(empirical_metadata, Data.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

data.AA.results <- data.frame() #empty place to store results

for (Journal in (c("Psychological Science",
                   "Clinical Psychological Science",
                   "Developmental Psychology",
                   "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                   "Journal of Personality and Social Psychology"))){
  print(Journal)
  Each_Journal <- subset(openaccess_data, Journals == Journal)

for (year in (c(2012, 2013, 2014, 2015))){
  print(year) #make sure loop is working
  papers <- subset(Each_Journal, grepl(year, Each_Journal$Article.ID.number)) #subsetting papers by year
  if (nrow(papers) > 0){
    papers <- subset(papers, papers[7] == "Yes")
  if (nrow(papers) > 0){
    intm <- aggregate(Did.the.article.receive.a.badge.for.open.data.~Are.the.data.located.at.the.working.page.,
                    data=papers, FUN=table) #of articles that had a data availability statement, are the data located where indicated. FUN is for summary statistics
    result <- data.frame(year, Journal, Are.the.data.located.at.the.working.page.=intm$Are.the.data.located.at.the.working.page.  ,
                       intm$Did.the.article.receive.a.badge.for.open.data.) #build data frame with year and title, =intm$ recall and produce output
    data.AA.results <- rbind(data.AA.results, result) #combine results together
  }
} #end of loop, signals to go back to next year
}
}
    
#reorganize the R output
DTotal.AA <- data.AA.results
Total <- rowSums(DTotal.AA[,c("Yes", "No")])
withtotal <- cbind(DTotal.AA, Total)
keeps <- c("year", "Journal", "Are.the.data.located.at.the.working.page.", "Total") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]

ActualAvailability.Data <- spread(keepcolumns, Are.the.data.located.at.the.working.page., Total)
ActualAvailability.Data[is.na(ActualAvailability.Data)] <- 0

colnames(ActualAvailability.Data)[3] <- "N/A" # Manual curation

colnames(ActualAvailability.Data) #  puts out the order so you can rearrange
ActualAvailability.Data <- ActualAvailability.Data[c(1,2,6,4,5,3)] # Rearrange columns so they match the order you want

#fill in years and journals where all content is 0
fill_empty <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(ActualAvailability.Data, Journal == Types)
  for (year_check in (c(2012, 2013, 2014, 2015))){
    Hold <- subset(Each_Journal, year == year_check)
    if (nrow(Hold) == 0){
      if ((Types == "Clinical Psychological Science") & (year_check == 2012)) {
        print(Types)
      }
      else{
        temporary <- data.frame(year_check, Types, 0,0,0,0)
        colnames(temporary) <- colnames(ActualAvailability.Data)
        fill_empty <- rbind(fill_empty, temporary)
      }
    }
  }
}

ActualAvailability.Data <- rbind(ActualAvailability.Data, fill_empty)
ActualAvailability.Data <- ActualAvailability.Data[order(ActualAvailability.Data$year),]

Dsummary.AA <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(ActualAvailability.Data, Journal == Types)
  if (nrow(Each_Journal) > 0){
    Hold <- subset(Each_Journal, year < 2014)
    if (nrow(Hold) > 0){
      Before_badges <- colSums(Hold[, c(3,4,5,6)])
      Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3],
                                  Before_badges[4])
      colnames(Before_badges) <- colnames(ActualAvailability.Data)
    }
    else{
      Before_badges <- data.frame("2012-2013", Types, 0,0,0,0)
      colnames(Before_badges) <- colnames(ActualAvailability.Data)
    }
    Hold <- subset(Each_Journal, year > 2013)
    if (nrow(Hold) > 0){
      After_badges <- colSums(Hold[, c(3,4,5,6)])
      After_badges <- data.frame("2014-2015", Types, After_badges[1], After_badges[2], After_badges[3],
                                 After_badges[4])
      colnames(After_badges) <- colnames(ActualAvailability.Data)
    }
    else{
      After_badges <- data.frame("2014-2015", Types, 0,0,0,0)
      colnames(After_badges) <- colnames(ActualAvailability.Data)
    }
  }
  else{
    Before_badges <- data.frame("2012-2013", Types, 0,0,0,0)
    colnames(Before_badges) <- colnames(ActualAvailability.Data)
    After_badges <- data.frame("2014-2015", Types, 0,0,0,0)
    colnames(After_badges) <- colnames(ActualAvailability.Data)
  }
  temporary <- rbind(Before_badges, After_badges)
  Dsummary.AA <- rbind(Dsummary.AA, temporary)
}

ActualAvailability.Data <- rbind(ActualAvailability.Data, Dsummary.AA)

ActualAvailability.Data <- ActualAvailability.Data[order(ActualAvailability.Data$Journal),]
rownames(ActualAvailability.Data) <- NULL

## Open dataframe "ActualAvailability.Data" for final output
## No data for CPS in 2012 is expected 

######################### ACTUAL AVAILABILITY: MATERIALS ##############################

# for loop to see within each year how many article received materials badge and if they were found at their location
openaccess_materials <- subset(empirical_metadata, Materials.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

materials.AA.results <- data.frame()

for (Journal in (c("Psychological Science",
                   "Clinical Psychological Science",
                   "Developmental Psychology",
                   "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                   "Journal of Personality and Social Psychology"))){
  print(Journal)
  Each_Journal <- subset(openaccess_materials, Journals == Journal)

  for (year in (c(2012, 2013, 2014, 2015))){
    print(year) #make sure loop is working
    papers <- subset(Each_Journal, grepl(year, Each_Journal$Article.ID.number)) #subsetting papers by year
    if (nrow(papers) > 0){
      papers <- subset(papers, papers[25] == "Yes")
      if (nrow(papers) > 0){
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.materials.~Are.the.materials.located.at.the.working.page.,
                    data=papers, FUN=table) #of articles that had a materials availability statement, are the materials located where indicated. FUN is for summary statistics
  result <- data.frame(year, Journal, Are.the.materials.located.at.the.working.page.=intm$Are.the.materials.located.at.the.working.page.  ,
                       intm$Did.the.article.receive.a.badge.for.open.materials.) #build data frame with year and title, =intm$ recall and produce output
  materials.AA.results <- rbind(materials.AA.results, result) #combine results together
      }
} #end of loop, signals to go back to next year
}
} 
    
#reorganize the R output
MTotal.AA <- materials.AA.results
Total <- rowSums(MTotal.AA[,c("Yes", "No")])
withtotal <- cbind(MTotal.AA, Total)
keeps <- c("year", "Journal", "Are.the.materials.located.at.the.working.page.", "Total") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]

ActualAvailability.Materials <- spread(keepcolumns, Are.the.materials.located.at.the.working.page., Total)
ActualAvailability.Materials[is.na(ActualAvailability.Materials)] <- 0

colnames(ActualAvailability.Materials)[3] <- "N/A" # Manual curation

colnames(ActualAvailability.Materials) #  puts out the order so you can rearrange
ActualAvailability.Materials <- ActualAvailability.Materials[c(1,2,6,4,5,3)] # Rearrange columns so they match the order you want

#fill in years and journals where all content is 0
fill_empty <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(ActualAvailability.Materials, Journal == Types)
  for (year_check in (c(2012, 2013, 2014, 2015))){
    Hold <- subset(Each_Journal, year == year_check)
    if (nrow(Hold) == 0){
      if ((Types == "Clinical Psychological Science") & (year_check == 2012)) {
        print(Types)
      }
      else{
        temporary <- data.frame(year_check, Types, 0,0,0,0)
        colnames(temporary) <- colnames(ActualAvailability.Materials)
        fill_empty <- rbind(fill_empty, temporary)
      }
    }
  }
}

ActualAvailability.Materials <- rbind(ActualAvailability.Materials, fill_empty)
ActualAvailability.Materials <- ActualAvailability.Materials[order(ActualAvailability.Materials$year),]

Msummary.AA <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(ActualAvailability.Materials, Journal == Types)
  if (nrow(Each_Journal) > 0){
    Hold <- subset(Each_Journal, year < 2014)
    if (nrow(Hold) > 0){
      Before_badges <- colSums(Hold[, c(3,4,5,6)])
      Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3],
                                  Before_badges[4])
      colnames(Before_badges) <- colnames(ActualAvailability.Materials)
    }
    else{
      Before_badges <- data.frame("2012-2013", Types, 0,0,0,0)
      colnames(Before_badges) <- colnames(ActualAvailability.Materials)
    }
    Hold <- subset(Each_Journal, year > 2013)
    if (nrow(Hold) > 0){
      After_badges <- colSums(Hold[, c(3,4,5,6)])
      After_badges <- data.frame("2014-2015", Types, After_badges[1], After_badges[2], After_badges[3],
                                 After_badges[4])
      colnames(After_badges) <- colnames(ActualAvailability.Materials)
    }
    else{
      After_badges <- data.frame("2014-2015", Types, 0,0,0,0)
      colnames(After_badges) <- colnames(ActualAvailability.Materials)
    }
  }
  else{
    Before_badges <- data.frame("2012-2013", Types, 0,0,0,0)
    colnames(Before_badges) <- colnames(ActualAvailability.Materials)
    After_badges <- data.frame("2014-2015", Types, 0,0,0,0)
    colnames(After_badges) <- colnames(ActualAvailability.Materials)
  }
  temporary <- rbind(Before_badges, After_badges)
  Msummary.AA <- rbind(Msummary.AA, temporary)
}

ActualAvailability.Materials <- rbind(ActualAvailability.Materials, Msummary.AA)

ActualAvailability.Materials <- ActualAvailability.Materials[order(ActualAvailability.Materials$Journal),]
rownames(ActualAvailability.Materials) <- NULL

## Open dataframe "ActualAvailability.Materials" for final output
## No data for CPS in 2012 is expected