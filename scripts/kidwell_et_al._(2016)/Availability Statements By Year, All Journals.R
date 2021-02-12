library(tidyr)
library(httr)

## THIS SCRIPT BREAKS DOWN NUMBER OF ARTICLES WITH DATA AND MATERIALS AVAILABILITY STATEMENTS BY YEAR AND JOURNAL.

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

################## AVAILABILITY STATEMENTS BY YEAR: DATA #########################

# for loop to see within each year how many articles shared data and received a badge
data.avail.year.results <- data.frame() #empty place to store results

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
    intm <- aggregate(Did.the.article.receive.a.badge.for.open.data.~Does.the.article.state.whether.or.not.the.data.are.available.+Data.statement.indicates.that.data.are.,
                      data=papers, FUN=table) #of articles that received a data badge (or not), did they have data availablility statement or not. FUN is for summary statistics
    result <- data.frame(Journal, year, Does.the.article.state.whether.or.not.the.data.are.available.=intm$Does.the.article.state.whether.or.not.the.data.are.available.,
                         Data.statement.indicates.that.data.are.=intm$Data.statement.indicates.that.data.are.,
                         intm$Did.the.article.receive.a.badge.for.open.data.) #build data frame with year and title, =intm$ recall and produce output
    data.avail.year.results <- rbind(data.avail.year.results, result) #combine results together
          }
      } #end of loop, signals to go back to next year
  }

#reorganize the R output
DTotal.Avail.Yr <- data.avail.year.results
Total <- rowSums(DTotal.Avail.Yr[,c("Yes", "No")])
withtotal <- cbind(DTotal.Avail.Yr, Total)
keeps <- c("year", "Journal", "Data.statement.indicates.that.data.are.", "Total") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]

AvailabilityStatement.Data.Year <- spread(keepcolumns, Data.statement.indicates.that.data.are., Total)
AvailabilityStatement.Data.Year[is.na(AvailabilityStatement.Data.Year)] <- 0

Dsummary.yr <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                   "Clinical Psychological Science",
                   "Developmental Psychology",
                   "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                   "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(AvailabilityStatement.Data.Year, Journal == Types)
  if (Types == "Clinical Psychological Science"){
    Before_badges <- colSums(Each_Journal[1, c(3,4,5)])
    Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3])
    colnames(Before_badges) <- colnames(AvailabilityStatement.Data.Year)
    After_badges <- colSums(Each_Journal[2:3, c(3,4,5)])
  }
  else{
    Before_badges <- colSums(Each_Journal[1:2, c(3,4,5)])
  Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2],Before_badges[3])
  colnames(Before_badges) <- colnames(AvailabilityStatement.Data.Year)
  After_badges <- colSums(Each_Journal[3:4, c(3,4,5)])
  }
  After_badges <- data.frame("2014-2015", Types, After_badges[1], After_badges[2], After_badges[3])
  colnames(After_badges) <- colnames(AvailabilityStatement.Data.Year)
  temporary <- rbind(Before_badges, After_badges)
  Dsummary.yr <- rbind(Dsummary.yr, temporary)
}

AvailabilityStatement.Data.Year <- rbind(AvailabilityStatement.Data.Year, Dsummary.yr)

AvailabilityStatement.Data.Year["Proportion Reportedly Available"] <- AvailabilityStatement.Data.Year$Available/rowSums(AvailabilityStatement.Data.Year[,3:5])

AvailabilityStatement.Data.Year <- AvailabilityStatement.Data.Year[order(AvailabilityStatement.Data.Year$Journal),]
rownames(AvailabilityStatement.Data.Year) <- NULL

## Open dataframe "AvailabilityStatement.Data.Year" for final output
## No data for CPS in 2012 is expected

################## AVAILABILITY STATEMENTS BY YEAR: MATERIALS #########################


# for loop to see within each year how many articles shared materials and received a badge
materials.avail.year.results <- data.frame() #empty place to store results

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
      intm <- aggregate(Did.the.article.receive.a.badge.for.open.materials.~Does.the.article.state.whether.or.not.any.research.materials.are.available.+Statement.indicates.that.materials.are.,
                        data=papers, FUN=table) #of articles that received a materials badge (or not), did they have materials availablility statement or not. FUN is for summary statistics
      result <- data.frame(Journal, year, Does.the.article.state.whether.or.not.any.research.materials.are.available.=intm$Does.the.article.state.whether.or.not.any.research.materials.are.available.,
                           Statement.indicates.that.materials.are.=intm$Statement.indicates.that.materials.are.,
                           intm$Did.the.article.receive.a.badge.for.open.materials.) #build data frame with year and title, =intm$ recall and produce output
      materials.avail.year.results <- rbind(materials.avail.year.results, result) #combine results together
    }
  } #end of loop, signals to go back to next year
  }

#reorganize the R output
MTotal.Avail.Yr <- materials.avail.year.results
Total <- rowSums(MTotal.Avail.Yr[,c("Yes", "No")])
withtotal <- cbind(MTotal.Avail.Yr, Total)
keeps <- c("year", "Journal", "Statement.indicates.that.materials.are.", "Total") # Change to columns you want to keep
keepcolumns <- withtotal[keeps]

AvailabilityStatement.Materials.Year <- spread(keepcolumns, Statement.indicates.that.materials.are., Total)
AvailabilityStatement.Materials.Year[is.na(AvailabilityStatement.Materials.Year)] <- 0

Msummary.yr <- data.frame() #empty place to store results

for (Types in (c("Psychological Science",
                 "Clinical Psychological Science",
                 "Developmental Psychology",
                 "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                 "Journal of Personality and Social Psychology"))){
  print(Types)
  Each_Journal <- subset(AvailabilityStatement.Materials.Year, Journal == Types)
  if (Types == "Clinical Psychological Science"){
    Before_badges <- colSums(Each_Journal[1, c(3,4)])
    Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2])
    colnames(Before_badges) <- colnames(AvailabilityStatement.Materials.Year)
    After_badges <- colSums(Each_Journal[2:3, c(3,4)])
  }
  else{
    Before_badges <- colSums(Each_Journal[1:2, c(3,4)])
    Before_badges <- data.frame("2012-2013", Types, Before_badges[1],Before_badges[2])
    colnames(Before_badges) <- colnames(AvailabilityStatement.Materials.Year)
    After_badges <- colSums(Each_Journal[3:4, c(3,4)])
  }
  After_badges <- data.frame("2014-2015", Types, After_badges[1], After_badges[2])
  colnames(After_badges) <- colnames(AvailabilityStatement.Materials.Year)
  temporary <- rbind(Before_badges, After_badges)
  Msummary.yr <- rbind(Msummary.yr, temporary)
}

AvailabilityStatement.Materials.Year <- rbind(AvailabilityStatement.Materials.Year, Msummary.yr)
AvailabilityStatement.Materials.Year[,"Unavailable"] <- 0

AvailabilityStatement.Materials.Year["Proportion Reportedly Available"] <- AvailabilityStatement.Materials.Year$Available/rowSums(AvailabilityStatement.Materials.Year[,3:5])

AvailabilityStatement.Materials.Year <- AvailabilityStatement.Materials.Year[order(AvailabilityStatement.Materials.Year$Journal),]
rownames(AvailabilityStatement.Materials.Year) <- NULL

## Open dataframe "AvailabilityStatement.Materials.Year" for final output
## No data for CPS in 2012 is expected