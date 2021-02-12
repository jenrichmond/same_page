install.packages("sqldf")
require(sqldf)

# Import tables
metadata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/JEPLMC Article List.csv", header=T, sep=","))
codeddata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Coded Article List.csv", header=T, sep=","))

# Remove any article IDs that are not from JEPLMC
JEPLMC_metadata <- subset(metadata, grepl(" JEPLMC", metadata$Article.ID.number))
JEPLMC_codeddata <- subset(codeddata, grepl(" JEPLMC", codeddata$Article.ID.number))

# Check to see if any article IDs that are in the metadata file are not in the coded data file
JEPLMC_metadata_Not_In_codeddata <- sqldf('SELECT * FROM JEPLMC_metadata EXCEPT SELECT * FROM JEPLMC_codeddata')
JEPLMC_metadata_Not_In_codeddata

# Check to see if any article IDs that are in the coded data file are not in the metadata file
JEPLMC_codeddata_Not_In_JEPLMC_metadata <- sqldf('SELECT * FROM JEPLMC_codeddata EXCEPT SELECT * FROM JEPLMC_metadata')
JEPLMC_codeddata_Not_In_JEPLMC_metadata

# Check to see what the overlap of article IDs are in the metadata and coded data file
JEPLMC_codeddata_In_JEPLMC_metadata <- sqldf('SELECT * FROM JEPLMC_codeddata INTERSECT SELECT * FROM JEPLMC_metadata')
JEPLMC_codeddata_In_JEPLMC_metadata

# Determine the number of unique article IDs in the metadata and coded data file
length(unique(JEPLMC_metadata$Article.ID.number))
length(unique(JEPLMC_codeddata$Article.ID.number))

# Determine the article IDs that are not unique in the coded data file
JEPLMC_codeddata[duplicated(JEPLMC_codeddata$Article.ID.number),]

# Clear all
rm(list = ls())