install.packages("sqldf")
require(sqldf)

# Import tables
metadata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/PS Article List.csv", header=T, sep=","))
codeddata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Coded Article List.csv", header=T, sep=","))

# Remove any article IDs that are not from Psychological Science
PS_metadata <- subset(metadata, grepl(" PS", metadata$Article.ID.number))
PS_codeddata <- subset(codeddata, grepl(" PS", codeddata$Article.ID.number))

# Check to see if any article IDs that are in the metadata file are not in the coded data file
PS_metadata_Not_In_codeddata <- sqldf('SELECT * FROM PS_metadata EXCEPT SELECT * FROM PS_codeddata')
PS_metadata_Not_In_codeddata

# Check to see if any article IDs that are in the coded data file are not in the metadata file
PS_codeddata_Not_In_PS_metadata <- sqldf('SELECT * FROM PS_codeddata EXCEPT SELECT * FROM PS_metadata')
PS_codeddata_Not_In_PS_metadata

# Check to see what the overlap of article IDs are in the metadata and coded data file
PS_codeddata_In_PS_metadata <- sqldf('SELECT * FROM PS_codeddata INTERSECT SELECT * FROM PS_metadata')
PS_codeddata_In_PS_metadata

# Determine the number of unique article IDs in the metadata and coded data file
length(unique(PS_metadata$Article.ID.number))
length(unique(PS_codeddata$Article.ID.number))

# Determine the article IDs that are not unique in the coded data file
PS_codeddata[duplicated(PS_codeddata$Article.ID.number),]

# Clear all
rm(list = ls())
