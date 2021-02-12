install.packages("sqldf")
require(sqldf)

# Import tables
metadata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/JPSP Article List.csv", header=T, sep=","))
codeddata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Coded Article List.csv", header=T, sep=","))

# Remove any article IDs that are not from JPSP
JPSP_metadata <- subset(metadata, grepl(" JPSP", metadata$Article.ID.number))
JPSP_codeddata <- subset(codeddata, grepl(" JPSP", codeddata$Article.ID.number))

# Check to see if any article IDs that are in the metadata file are not in the coded data file
JPSP_metadata_Not_In_codeddata <- sqldf('SELECT * FROM JPSP_metadata EXCEPT SELECT * FROM JPSP_codeddata')
JPSP_metadata_Not_In_codeddata

# Check to see if any article IDs that are in the coded data file are not in the metadata file
JPSP_codeddata_Not_In_JPSP_metadata <- sqldf('SELECT * FROM JPSP_codeddata EXCEPT SELECT * FROM JPSP_metadata')
JPSP_codeddata_Not_In_JPSP_metadata

# Check to see what the overlap of article IDs are in the metadata and coded data file
JPSP_codeddata_In_JPSP_metadata <- sqldf('SELECT * FROM JPSP_codeddata INTERSECT SELECT * FROM JPSP_metadata')
JPSP_codeddata_In_JPSP_metadata

# Determine the number of unique article IDs in the metadata and coded data file
length(unique(JPSP_metadata$Article.ID.number))
length(unique(JPSP_codeddata$Article.ID.number))

# Determine the article IDs that are not unique in the coded data file
JPSP_codeddata[duplicated(JPSP_codeddata$Article.ID.number),]

# Clear all
rm(list = ls())
