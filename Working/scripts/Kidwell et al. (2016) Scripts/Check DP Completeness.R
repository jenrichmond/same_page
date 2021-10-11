install.packages("sqldf")
require(sqldf)

# Import tables
metadata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/DP Article List.csv", header=T, sep=","))
codeddata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Coded Article List.csv", header=T, sep=","))

# Remove any article IDs that are not from Developmental Psychology
DP_metadata <- subset(metadata, grepl(" DP", metadata$Article.ID.number))
DP_codeddata <- subset(codeddata, grepl(" DP", codeddata$Article.ID.number))

# Check to see if any article IDs that are in the metadata file are not in the coded data file
DP_metadata_Not_In_codeddata <- sqldf('SELECT * FROM DP_metadata EXCEPT SELECT * FROM DP_codeddata')
DP_metadata_Not_In_codeddata

# Check to see if any article IDs that are in the coded data file are not in the metadata file
DP_codeddata_Not_In_DP_metadata <- sqldf('SELECT * FROM DP_codeddata EXCEPT SELECT * FROM DP_metadata')
DP_codeddata_Not_In_DP_metadata

# Check to see what the overlap of article IDs are in the metadata and coded data file
DP_codeddata_In_DP_metadata <- sqldf('SELECT * FROM DP_codeddata INTERSECT SELECT * FROM DP_metadata')
DP_codeddata_In_DP_metadata

# Determine the number of unique article IDs in the metadata and coded data file
length(unique(DP_metadata$Article.ID.number))
length(unique(DP_codeddata$Article.ID.number))

# Determine the article IDs that are not unique in the coded data file
DP_codeddata[duplicated(DP_codeddata$Article.ID.number),]

# Clear all
rm(list = ls())