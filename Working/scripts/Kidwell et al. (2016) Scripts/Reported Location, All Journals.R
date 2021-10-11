library(tidyr)
library(httr)

## THIS SCRIPT BREAKS DOWN THE REPORTED LOCATION OF DATA AND MATERIALS WITH AVAILABILITY STATEMENTS BY YEAR AND JOURNAL.

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

################## REPORTED LOCATION: DATA #########################

# for loop to see within each year how many articles shared data and received a badge
data.RL.results <- data.frame() #empty place to store results

for (Journal in (c("Psychological Science",
                   "Clinical Psychological Science",
                   "Developmental Psychology",
                   "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                   "Journal of Personality and Social Psychology"))){
  print(Journal)
  Each_Journal <- subset(empirical_metadata, Journals == Journal)
  
  for (year in (c(2012, 2013, 2014, 2015))){
    print(year) #make sure loop is working
    papers <- subset(Each_Journal, grepl(year, Each_Journal$Article.ID.number)) #subsetting papers by year
    if (nrow(papers) > 0){
    papers <- subset(papers, papers[7] == "Yes")
    if (nrow(papers) > 0){
      intm <- aggregate(Did.the.article.receive.a.badge.for.open.data.~Data.URL.links.to.,
                        data=papers, FUN=table) #of articles that had a data badge, how were data accessible. FUN is for summary statistics
      result <- data.frame(year, Journal, Data.URL.links.to.=intm$Data.URL.links.to.  ,
                           intm$Did.the.article.receive.a.badge.for.open.data.) #build data frame with year and title, =intm$ recall and produce output
      data.RL.results <- rbind(data.RL.results, result) #combine results together
    }
    }
  }
}

## Adding Journals and Years where there are known zeroes
result <- data.frame(c("2015","2014","2015"),
                     c("Clinical Psychological Science",
                     "Clinical Psychological Science",
                     "Journal of Personality and Social Psychology"),
                     c(rep(NA,3)),c(rep(NA,3)),c(rep(NA,3)))
colnames(result) <- c("year","Journal","Data.URL.links.to.","No","Yes")
data.RL.results <- rbind(data.RL.results, result) #combine results together

### reorganize the R output

DTotal.RL <- data.RL.results
Total <- rowSums(DTotal.RL[,c("Yes", "No")])
withtotal <- cbind(DTotal.RL, Total)
keeps <- c("year", "Journal", "Data.URL.links.to.", "Total") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]

ReportedLocation.Data <- spread(keepcolumns, Data.URL.links.to., Total)
ReportedLocation.Data[is.na(ReportedLocation.Data)] <- 0

ReportedLocation.Data <- cbind(row.names(ReportedLocation.Data),
                              ReportedLocation.Data[1],
                              ReportedLocation.Data[2],
                              ReportedLocation.Data[3],
                              ReportedLocation.Data[5],
                              ReportedLocation.Data[8],
                              ReportedLocation.Data[9],
                              ReportedLocation.Data[11],
                              data.frame(Journal.Supplement=(rowSums(ReportedLocation.Data[c(-1,-2,-3,-5,-8,-9,-11)]))))
ReportedLocation.Data$`row.names(ReportedLocation.Data)` <- NULL
colnames(ReportedLocation.Data)[3] <- "Other" # Manual curation

colnames(ReportedLocation.Data) #  puts out the order so you can rearrange
ReportedLocation.Data <- ReportedLocation.Data[c(1,2,4,8,7,6,5,3)] # Rearrange columns so they match the order you want
colnames(ReportedLocation.Data) <- c("Year", "Journal", "Independent archive / repository","Journal Supplement",
                            "Independent Website","Personal Website","Appendix or Table","Other")

Dsummary.RL <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(ReportedLocation.Data, Journal == Types)
  Hold <- subset(Each_Journal, Year < 2014)
  Before_badges <- colSums(Hold[, c(3,4,5,6,7,8)])
  Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3],
                              Before_badges[4],Before_badges[5],Before_badges[6])
  colnames(Before_badges) <- colnames(ReportedLocation.Data)
  Hold_again <- subset(Each_Journal, Year > 2013)
  After_badges <- colSums(Hold_again[, c(3,4,5,6,7,8)])
  After_badges <- data.frame("2014-2015", Types, After_badges[1],After_badges[2],After_badges[3],
                             After_badges[4],After_badges[5],After_badges[6])
  colnames(After_badges) <- colnames(ReportedLocation.Data)
  temporary <- rbind(Before_badges, After_badges)
  Dsummary.RL <- rbind(Dsummary.RL, temporary)
}

ReportedLocation.Data <- rbind(ReportedLocation.Data, Dsummary.RL)

ReportedLocation.Data <- ReportedLocation.Data[order(ReportedLocation.Data$Journal),]
rownames(ReportedLocation.Data) <- NULL

## Open dataframe "ReportedLocation.Data" for final output
## No data for CPS in 2012 is expected 


################## REPORTED LOCATION: MATERIALS #########################

# for loop to see within each year how many articles shared data and received a badge
materials.RL.results <- data.frame() #empty place to store results

for (Journal in (c("Psychological Science",
                   "Clinical Psychological Science",
                   "Developmental Psychology",
                   "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                   "Journal of Personality and Social Psychology"))){
  print(Journal)
  Each_Journal <- subset(empirical_metadata, Journals == Journal)

  for (year in (c(2012, 2013, 2014, 2015))){
    print(year) #make sure loop is working
    papers <- subset(Each_Journal, grepl(year, Each_Journal$Article.ID.number)) #subsetting papers by year
    if (nrow(papers) > 0){
      papers <- subset(papers, papers[25] == "Yes")
      if (nrow(papers) > 0){
        intm <- aggregate(Did.the.article.receive.a.badge.for.open.materials.~Materials.URL.links.to. ,
                          data=papers, FUN=table)
        result <- data.frame(year, Journal, Materials.URL.links.to.=intm$Materials.URL.links.to.  ,
                             intm$Did.the.article.receive.a.badge.for.open.materials.)
        materials.RL.results <- rbind(materials.RL.results, result)
      }
    } #end of loop, signals to go back to next year
  }
  }

### reorganize the R output

MTotal.RL <- materials.RL.results
Total <- rowSums(MTotal.RL[,c("Yes", "No")])
withtotal <- cbind(MTotal.RL, Total)
keeps <- c("year", "Journal", "Materials.URL.links.to.", "Total") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]

ReportedLocation.Materials <- spread(keepcolumns, Materials.URL.links.to., Total)
ReportedLocation.Materials[is.na(ReportedLocation.Materials)] <- 0

ReportedLocation.Materials <- cbind(row.names(ReportedLocation.Materials),
                              ReportedLocation.Materials[1],
                              ReportedLocation.Materials[2],
                              ReportedLocation.Materials[3],
                              ReportedLocation.Materials[6],
                              ReportedLocation.Materials[9],
                              ReportedLocation.Materials[10],
                              ReportedLocation.Materials[12],
                              data.frame(Journal.Supplement=(rowSums(ReportedLocation.Materials[c(-1,-2,-3,-6,-9,-10,-12)]))))
ReportedLocation.Materials$`row.names(ReportedLocation.Materials)` <- NULL
colnames(ReportedLocation.Materials)[3] <- "Other" # Manual curation

colnames(ReportedLocation.Materials) #  puts out the order so you can rearrange
ReportedLocation.Materials <- ReportedLocation.Materials[c(1,2,4,8,7,6,5,3)] # Rearrange columns so they match the order you want
colnames(ReportedLocation.Materials) <- c("Year", "Journal", "Independent archive / repository","Journal Supplement",
                                    "Independent Website","Personal Website","Appendix or Table","Other")


Msummary.RL <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(ReportedLocation.Materials, Journal == Types)
  if (Types == "Clinical Psychological Science"){
    Before_badges <- colSums(Each_Journal[1, c(3,4,5,6,7,8)])
    Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3],
                                Before_badges[4],Before_badges[5],Before_badges[6])
    colnames(Before_badges) <- colnames(ReportedLocation.Materials)
    After_badges <- colSums(Each_Journal[2:3, c(3,4,5,6,7,8)])
  }
  else{
    Before_badges <- colSums(Each_Journal[1:2, c(3,4,5,6,7,8)])
    Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3],
                                Before_badges[4],Before_badges[5],Before_badges[6])
    colnames(Before_badges) <- colnames(ReportedLocation.Materials)
    After_badges <- colSums(Each_Journal[3:4, c(3,4,5,6,7,8)])
  }
  After_badges <- data.frame("2014-2015", Types, After_badges[1], After_badges[2], After_badges[3],
                             After_badges[4], After_badges[5], After_badges[6])
  colnames(After_badges) <- colnames(ReportedLocation.Materials)
  temporary <- rbind(Before_badges, After_badges)
  Msummary.RL <- rbind(Msummary.RL, temporary)
}

ReportedLocation.Materials <- rbind(ReportedLocation.Materials, Msummary.RL)

ReportedLocation.Materials <- ReportedLocation.Materials[order(ReportedLocation.Materials$Journal),]
rownames(ReportedLocation.Materials) <- NULL

## Open dataframe "ReportedLocation.Materials" for final output
## No data for CPS in 2012 is expected 