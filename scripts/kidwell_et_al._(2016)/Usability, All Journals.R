library(tidyr)
library(httr)

## THIS SCRIPT BREAKS DOWN THE USABILITY OF DATA AND MATERIALS REPORTEDLY AVAILABLE AT OPEN ACCESS LOCATIONS BY YEAR AND JOURNAL.

## NOTE: THIS SCRIPT WILL AUTOMATICALLY IMPORT THE DATA FILE NECESSARY TO RUN THIS SCRIPT, "Master Dataset.csv," FROM THE OPEN SCIENCE FRAMEWORK (OSF).

## IF YOU WOULD PREFER TO MANUALLY DOWNLOAD THE DATA FILE TO YOUR MACHINE AND IMPORT INTO R, PLEASE:
## 1. DOWNLOAD "Master Dataset.csv" FROM THE OPEN SCIENCE FRAMEWORK: https://osf.io/a29bt/
## 2. DO NOT RUN LINE 14
## 3. MODIFY LINE 15 TO READ THE LOCATION OF THE FILE ON YOUR MACHINE.

# Import tables from OSF file "Master Dataset.csv"
metadata <- GET('https://osf.io/a29bt/?action=download', write_disk('Master Dataset.csv', overwrite = TRUE))
metadata <- as.data.frame(read.csv("Master Dataset.csv", header=T, sep=","))

empirical_metadata <- subset(metadata, Number.of.experiments > 0)

# Assign journal names based on Article ID Number
empirical_metadata$Journals <- ifelse(grepl(" PS", empirical_metadata$Article.ID.number), "Psychological Science",
                                      ifelse(grepl(" CPS", empirical_metadata$Article.ID.number), "Clinical Psychological Science",
                                             ifelse(grepl(" DP", empirical_metadata$Article.ID.number), "Developmental Psychology",
                                                    ifelse(grepl(" JEPLMC", empirical_metadata$Article.ID.number), "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                                                           ifelse(grepl(" JPSP", empirical_metadata$Article.ID.number), "Journal of Personality and Social Psychology",
                                                                  NA  )))))

###################################### USABILITY: DATA ##############################################

# for loop to see within each year how many article received data badge and if they were understandable/usable
openaccess_data <- subset(empirical_metadata, Data.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

data.usability.results <- data.frame() #empty place to store results

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
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.data.~Are.the.data.understandable.and.usable.after.brief.review.,
                    data=papers, FUN=table) #of articles that had a data availability statement, are the data complete. FUN is for summary statistics
  result <- data.frame(year, Journal, Are.the.data.understandable.and.usable.after.brief.review.=intm$Are.the.data.understandable.and.usable.after.brief.review.  ,
                       intm$Did.the.article.receive.a.badge.for.open.data.) #build data frame with year and title, =intm$ recall and produce output
  data.usability.results <- rbind(data.usability.results, result) #combine results together
      }
    } #end of loop, signals to go back to next year
  }
  }

#reorganize the R output
DTotal.Usability <- data.usability.results
Total <- rowSums(DTotal.Usability[,c("Yes", "No")])
withtotal <- cbind(DTotal.Usability, Total)
keeps <- c("year", "Journal", "Are.the.data.understandable.and.usable.after.brief.review.", "Total") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]

Usability.Data <- spread(keepcolumns, Are.the.data.understandable.and.usable.after.brief.review., Total)
Usability.Data[is.na(Usability.Data)] <- 0

colnames(Usability.Data)[3] <- "N/A" # Manual curation

colnames(Usability.Data) #  puts out the order so you can rearrange
Usability.Data <- Usability.Data[c(1,2,6,4,5,3)] # Rearrange columns so they match the order you want

#fill in years and journals where all content is 0
fill_empty <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(Usability.Data, Journal == Types)
  for (year_check in (c(2012, 2013, 2014, 2015))){
    Hold <- subset(Each_Journal, year == year_check)
    if (nrow(Hold) == 0){
      if ((Types == "Clinical Psychological Science") & (year_check == 2012)) {
        print(Types)
      }
      else{
        temporary <- data.frame(year_check, Types, 0,0,0,0)
        colnames(temporary) <- colnames(Usability.Data)
        fill_empty <- rbind(fill_empty, temporary)
      }
    }
  }
}

Usability.Data <- rbind(Usability.Data, fill_empty)
Usability.Data <- Usability.Data[order(Usability.Data$year),]

Dsummary.Usability <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(Usability.Data, Journal == Types)
  if (nrow(Each_Journal) > 0){
    Hold <- subset(Each_Journal, year < 2014)
    if (nrow(Hold) > 0){
      Before_badges <- colSums(Hold[, c(3,4,5,6)])
      Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3],
                                  Before_badges[4])
      colnames(Before_badges) <- colnames(Usability.Data)
    }
    else{
      Before_badges <- data.frame("2012-2013", Types, 0,0,0,0)
      colnames(Before_badges) <- colnames(Usability.Data)
    }
    Hold <- subset(Each_Journal, year > 2013)
    if (nrow(Hold) > 0){
      After_badges <- colSums(Hold[, c(3,4,5,6)])
      After_badges <- data.frame("2014-2015", Types, After_badges[1], After_badges[2], After_badges[3],
                                 After_badges[4])
      colnames(After_badges) <- colnames(Usability.Data)
    }
    else{
      After_badges <- data.frame("2014-2015", Types, 0,0,0,0)
      colnames(After_badges) <- colnames(Usability.Data)
    }
  }
  else{
    Before_badges <- data.frame("2012-2013", Types, 0,0,0,0)
    colnames(Before_badges) <- colnames(Usability.Data)
    After_badges <- data.frame("2014-2015", Types, 0,0,0,0)
    colnames(After_badges) <- colnames(Usability.Data)
  }
  temporary <- rbind(Before_badges, After_badges)
  Dsummary.Usability <- rbind(Dsummary.Usability, temporary)
}

Usability.Data <- rbind(Usability.Data, Dsummary.Usability)

Usability.Data <- Usability.Data[order(Usability.Data$Journal),]
rownames(Usability.Data) <- NULL

## Open dataframe "Usability.Data" for final output
## No data for CPS in 2012 is expected 


###################################### USABILITY: MATERIALS ##############################################

# for loop to see within each year how many article received materials badge and if they were understandable/usable
openaccess_materials <- subset(empirical_metadata, Materials.URL.links.to. %in% c("Independent archive / repository", "Personal site", "Third party site"))

materials.usability.results <- data.frame()

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
  intm <- aggregate(Did.the.article.receive.a.badge.for.open.materials.~Are.the.materials.understandable.and.usable.after.brief.review.,
                    data=papers, FUN=table) #of articles that had a materials availability statement, are the materials located where indicated. FUN is for summary statistics
  result <- data.frame(year, Journal, Are.the.materials.understandable.and.usable.after.brief.review.=intm$Are.the.materials.understandable.and.usable.after.brief.review.  ,
                       intm$Did.the.article.receive.a.badge.for.open.materials.) #build data frame with year and title, =intm$ recall and produce output
  materials.usability.results <- rbind(materials.usability.results, result) #combine results together
      }
    } #end of loop, signals to go back to next year
  }
  }   

#reorganize the R output
MTotal.Usability <- materials.usability.results
Total <- rowSums(MTotal.Usability[,c("Yes", "No")])
withtotal <- cbind(MTotal.Usability, Total)
keeps <- c("year", "Journal", "Are.the.materials.understandable.and.usable.after.brief.review.", "Total") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]

Usability.Materials <- spread(keepcolumns, Are.the.materials.understandable.and.usable.after.brief.review., Total)
Usability.Materials[is.na(Usability.Materials)] <- 0

colnames(Usability.Materials)[3] <- "N/A" # Manual curation

colnames(Usability.Materials) #  puts out the order so you can rearrange
Usability.Materials <- Usability.Materials[c(1,2,6,4,5,3)] # Rearrange columns so they match the order you want

#fill in years and journals where all content is 0
fill_empty <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(Usability.Materials, Journal == Types)
  for (year_check in (c(2012, 2013, 2014, 2015))){
    Hold <- subset(Each_Journal, year == year_check)
    if (nrow(Hold) == 0){
      if ((Types == "Clinical Psychological Science") & (year_check == 2012)) {
        print(Types)
      }
      else{
        temporary <- data.frame(year_check, Types, 0,0,0,0)
        colnames(temporary) <- colnames(Usability.Materials)
        fill_empty <- rbind(fill_empty, temporary)
      }
    }
  }
}

Usability.Materials <- rbind(Usability.Materials, fill_empty)
Usability.Materials <- Usability.Materials[order(Usability.Materials$year),]

Msummary.Usability <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(Usability.Materials, Journal == Types)
  if (nrow(Each_Journal) > 0){
    Hold <- subset(Each_Journal, year < 2014)
    if (nrow(Hold) > 0){
      Before_badges <- colSums(Hold[, c(3,4,5,6)])
      Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3],
                                  Before_badges[4])
      colnames(Before_badges) <- colnames(Usability.Materials)
    }
    else{
      Before_badges <- data.frame("2012-2013", Types, 0,0,0,0)
      colnames(Before_badges) <- colnames(Usability.Materials)
    }
    Hold <- subset(Each_Journal, year > 2013)
    if (nrow(Hold) > 0){
      After_badges <- colSums(Hold[, c(3,4,5,6)])
      After_badges <- data.frame("2014-2015", Types, After_badges[1], After_badges[2], After_badges[3],
                                 After_badges[4])
      colnames(After_badges) <- colnames(Usability.Materials)
    }
    else{
      After_badges <- data.frame("2014-2015", Types, 0,0,0,0)
      colnames(After_badges) <- colnames(Usability.Materials)
    }
  }
  else{
    Before_badges <- data.frame("2012-2013", Types, 0,0,0,0)
    colnames(Before_badges) <- colnames(Usability.Materials)
    After_badges <- data.frame("2014-2015", Types, 0,0,0,0)
    colnames(After_badges) <- colnames(Usability.Materials)
  }
  temporary <- rbind(Before_badges, After_badges)
  Msummary.Usability <- rbind(Msummary.Usability, temporary)
}

Usability.Materials <- rbind(Usability.Materials, Msummary.Usability)

Usability.Materials <- Usability.Materials[order(Usability.Materials$Journal),]
rownames(Usability.Materials) <- NULL

## Open dataframe "Usability.Materials" for final output
## No data for CPS in 2012 is expected 