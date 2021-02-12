library(tidyr)
library(httr)

## THIS SCRIPT BREAKS DOWN NUMBER OF ARTICLES WITH DATA AND MATERIALS AVAILABILITY STATEMENTS BY 6 MONTH PERIODS AND JOURNAL.

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

# for loop to see within each year how many article shared data and received a badge
data.breakdown <- data.frame() #empty place to store results

for (Journal in (c("Psychological Science",
                   "Clinical Psychological Science",
                   "Developmental Psychology",
                   "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                   "Journal of Personality and Social Psychology"))){
  print(Journal)
  Each_Journal <- subset(empirical_metadata, Journals == Journal)

  for (year in (c(2012, 2013, 2014, 2015))){
    print(year)
    papers <- subset(Each_Journal, grepl(year, Each_Journal$Article.ID.number)) # subset on year
    begin <- subset(papers, grepl("-1-|-2-|-3-|-4-|-5-|-6-",papers$Article.ID.number)) # subset on first 6 months
    begin$breakdown <- rep(paste("begin", year),nrow(begin)) # add column to those articles
    if(year<2015){
      print(year)
      end <- subset(papers, grepl("-7-|-8-|-9-|-10-|-11-|-12-",papers$Article.ID.number)) # subset on last 6 months
      end$breakdown <- rep(paste("end", year),nrow(end)) # add column to those articles
      result <- rbind(begin, end)
    } else {
      result <- begin
    }
    data.breakdown <- rbind(data.breakdown, result)
  }
}
  
 number_articles <- as.data.frame(with(data.breakdown, table(breakdown))) # frequency of articles in each 6 month period
 
 ################################### AVAILABILITY BY 6 MONTHS: DATA ########################################
 
 # for loop to see within each year how many article shared data and received a badge
 data.avail.6M.results <- data.frame() #empty place to store results
 
 for (Journal in (c("Psychological Science",
                    "Clinical Psychological Science",
                    "Developmental Psychology",
                    "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                    "Journal of Personality and Social Psychology"))){
   print(Journal)
   Each_Journal <- subset(data.breakdown, Journals == Journal)
 
 for (year_half in (c("begin 2012", "end 2012", "begin 2013", "end 2013", "begin 2014", "end 2014", "begin 2015"))){
   print(year_half) #make sure loop is working
   papers <- subset(Each_Journal, breakdown == year_half) #subsetting papers by year
   if (nrow(papers) > 0){
   intm <- aggregate(Did.the.article.receive.a.badge.for.open.data.~Does.the.article.state.whether.or.not.the.data.are.available.+Data.statement.indicates.that.data.are.,
                     data=papers, FUN=table) #of articles that received a data badge (or not), did they have data availablility statement or not. FUN is for summary statistics
   result <- data.frame(year_half, Journal, Does.the.article.state.whether.or.not.the.data.are.available.=intm$Does.the.article.state.whether.or.not.the.data.are.available.,
                        Data.statement.indicates.that.data.are.=intm$Data.statement.indicates.that.data.are.,
                        intm$Did.the.article.receive.a.badge.for.open.data.) #build data frame with year and title, =intm$ recall and produce output
   data.avail.6M.results <- rbind(data.avail.6M.results, result) #combine results together
 }
}#end of loop, signals to go back to next year
 }
 
 #reorganize the R output
 DTotal.Avail.6M <- data.avail.6M.results
 Total <- rowSums(DTotal.Avail.6M[,c("Yes", "No")])
 withtotal <- cbind(DTotal.Avail.6M, Total)
 keeps <- c("year_half", "Journal", "Data.statement.indicates.that.data.are.", "Total") # Change to columns you want to keep
 keepcolumns <- withtotal[keeps]
 
 AvailabilityStatements.Data.6M <- spread(keepcolumns, Data.statement.indicates.that.data.are., Total)
 AvailabilityStatements.Data.6M[is.na(AvailabilityStatements.Data.6M)] <- 0
 
 AvailabilityStatements.Data.6M["Proportion Reportedly Available"] <- AvailabilityStatements.Data.6M$Available/rowSums(AvailabilityStatements.Data.6M[,3:5])
 
 AvailabilityStatements.Data.6M <- AvailabilityStatements.Data.6M[order(AvailabilityStatements.Data.6M$Journal),]
 rownames(AvailabilityStatements.Data.6M) <- NULL
 
 ## Open dataframe "AvailabilityStatement.Data.6M" for final output
 ## No data for CPS in 2012 is expected
 
 ################################### AVAILABILITY BY 6 MONTHS: MATERIALS ########################################
 
 # for loop to see within each year how many article shared data and received a badge
 materials.avail.6M.results <- data.frame() #empty place to store results
 
 for (Journal in (c("Psychological Science",
                    "Clinical Psychological Science",
                    "Developmental Psychology",
                    "Journal of Experimental Psychology: Learning, Memory, and Cognition",
                    "Journal of Personality and Social Psychology"))){
   print(Journal)
   Each_Journal <- subset(data.breakdown, Journals == Journal)
   
   for (year_half in (c("begin 2012", "end 2012", "begin 2013", "end 2013", "begin 2014", "end 2014", "begin 2015"))){
     print(year_half) #make sure loop is working
     papers <- subset(Each_Journal, breakdown == year_half) #subsetting papers by year
     if (nrow(papers) > 0){
     intm <- aggregate(Did.the.article.receive.a.badge.for.open.materials.~Does.the.article.state.whether.or.not.any.research.materials.are.available.+Statement.indicates.that.materials.are.,
                       data=papers, FUN=table) #of articles that received a data badge (or not), did they have data availablility statement or not. FUN is for summary statistics
     result <- data.frame(year_half, Journal, Does.the.article.state.whether.or.not.any.research.materials.are.available.=intm$Does.the.article.state.whether.or.not.any.research.materials.are.available.,
                          Statement.indicates.that.materials.are.=intm$Statement.indicates.that.materials.are.,
                          intm$Did.the.article.receive.a.badge.for.open.materials.) #build data frame with year and title, =intm$ recall and produce output
     materials.avail.6M.results <- rbind( materials.avail.6M.results, result) #combine results together
   }
  }#end of loop, signals to go back to next year
 }
 
 #reorganize the R output
 MTotal.Avail.6M <- materials.avail.6M.results
 Total <- rowSums(MTotal.Avail.6M[,c("Yes", "No")])
 withtotal <- cbind(MTotal.Avail.6M, Total)
 keeps <- c("year_half", "Journal", "Statement.indicates.that.materials.are.", "Total") # Change to columns you want to keep
 keepcolumns <- withtotal[keeps]
 
 AvailabilityStatements.Materials.6M <- spread(keepcolumns, Statement.indicates.that.materials.are., Total)
 AvailabilityStatements.Materials.6M[is.na(AvailabilityStatements.Materials.6M)] <- 0
 AvailabilityStatements.Materials.6M[,"Unavailable"] <- 0
 
 AvailabilityStatements.Materials.6M["Proportion Reportedly Available"] <- AvailabilityStatements.Materials.6M$Available/rowSums(AvailabilityStatements.Materials.6M[,3:5])
 
 AvailabilityStatements.Materials.6M <- AvailabilityStatements.Materials.6M[order(AvailabilityStatements.Materials.6M$Journal),]
 rownames(AvailabilityStatements.Materials.6M) <- NULL
 
 ## Open dataframe "AvailabilityStatement.Materials.6M" for final output
 ## No data for CPS in 2012 is expected 