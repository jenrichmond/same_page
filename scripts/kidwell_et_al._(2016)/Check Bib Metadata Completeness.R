install.packages("sqldf")
require(sqldf)

# Import tables
Bib_metadata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Bibliographic Metadata/Bib Metadata Article List.csv", header=T, sep=","))
Bib_codeddata <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Coded Article List.csv", header=T, sep=","))
Bib_titles <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Bibliographic Metadata/Bib Metadata Titles.csv", header=T, sep=","))
Bib_DOI <- as.data.frame(read.csv("/Users/mallorykidwell/Desktop/Effect of Badges Analyses - R/Check for Completeness/Article Lists/Bibliographic Metadata/Bib Metadata DOI.csv", header=T, sep=","))

# Check to see if any article IDs that are in the metadata file are not in the coded data file
Bib_metadata_Not_In_codeddata <- sqldf('SELECT * FROM Bib_metadata EXCEPT SELECT * FROM Bib_codeddata')
Bib_metadata_Not_In_codeddata

# Check to see if any article IDs that are in the coded data file are not in the metadata file
Bib_codeddata_Not_In_Bib_metadata <- sqldf('SELECT * FROM Bib_codeddata EXCEPT SELECT * FROM Bib_metadata')
Bib_codeddata_Not_In_Bib_metadata

# Check to see what the overlap of article IDs are in the metadata and coded data file
Bib_codeddata_In_Bib_metadata <- sqldf('SELECT * FROM Bib_codeddata INTERSECT SELECT * FROM Bib_metadata')
Bib_codeddata_In_Bib_metadata

# Determine the number of unique article IDs in the metadata and coded data file
length(unique(Bib_metadata$Article.ID))
length(unique(Bib_codeddata$Article.ID.number))

# Determine the article IDs that are not unique in the coded data file
Bib_codeddata[duplicated(Bib_codeddata$Article.ID.number),]

# Determine the article IDs that are not unique in the metadata file
Bib_metadata[duplicated(Bib_metadata$Article.ID),]

# Determine the article titles that are not unique in the bib metadata file
Bib_titles[duplicated(Bib_titles$Title),]

# Determine the article titles that are not unique in the bib metadata file
Bib_DOI[duplicated(Bib_DOI$DOI),]

# Clear all
rm(list = ls())