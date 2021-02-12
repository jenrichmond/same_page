install.packages("sqldf")
require(sqldf)

# Import tables
metadata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/CPS Article List.csv", header=T, sep=","))
codeddata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Coded Article List.csv", header=T, sep=","))

# Remove any article IDs that are not from CPS
CPS_metadata <- subset(metadata, grepl(" CPS", metadata$Article.ID.number))
CPS_codeddata <- subset(codeddata, grepl(" CPS", codeddata$Article.ID.number))

# Check to see if any article IDs that are in the metadata file are not in the coded data file
CPS_metadata_Not_In_codeddata <- sqldf('SELECT * FROM CPS_metadata EXCEPT SELECT * FROM CPS_codeddata')
CPS_metadata_Not_In_codeddata

# Check to see if any article IDs that are in the coded data file are not in the metadata file
CPS_codeddata_Not_In_CPS_metadata <- sqldf('SELECT * FROM CPS_codeddata EXCEPT SELECT * FROM CPS_metadata')
CPS_codeddata_Not_In_CPS_metadata

# Check to see what the overlap of article IDs are in the metadata and coded data file
CPS_codeddata_In_CPS_metadata <- sqldf('SELECT * FROM CPS_codeddata INTERSECT SELECT * FROM CPS_metadata')
CPS_codeddata_In_CPS_metadata

# Determine the number of unique article IDs in the metadata and coded data file
length(unique(CPS_metadata$Article.ID.number))
length(unique(CPS_codeddata$Article.ID.number))

# Determine the article IDs that are not unique in the coded data file
CPS_codeddata[duplicated(CPS_codeddata$Article.ID.number),]

# Clear all
rm(list = ls())