# Load required packages
if (!require("psych")) install.packages("psych", dependencies=TRUE)
if (!require("GPArotation")) install.packages("GPArotation", dependencies=TRUE)
library(psych)
library(GPArotation)

# Read CSV data
data <- read.csv("data_R.csv")

# Remove metadata columns
data_clean <- data[, !(names(data) %in% c("Participant", "NoteTakingTime", "RetrievalTime", "Preference"))]

# Convert to numeric and drop non-numeric columns
data_clean[] <- lapply(data_clean, function(x) as.numeric(as.character(x)))
data_clean <- data_clean[, colSums(is.na(data_clean)) == 0]

# Drop columns with no variance
data_clean <- data_clean[, apply(data_clean, 2, function(x) var(x, na.rm = TRUE) > 0)]

# Correlation matrix
cor_matrix <- cor(data_clean)

# Scree plot
scree(cor_matrix)

# EFA - Choose number of factors (example: 1 factor)
efa_result <- fa(r = cor_matrix, nfactors = 1, rotate = "varimax", fm = "pa")

# Print EFA result
print(efa_result)

#Extracted one meaningful factor, which we can label as “Retrieval Effectiveness.” This factor:

#Explains ~25% of the variance in the data.

#Is primarily represented by these items:
  
#Retrieval2: Strong loading (0.81)

#Retrieval3: Moderate loading (0.59)

#General2: Moderate loading (0.57)

#Materiality1: Lower but still meaningful (0.38)

# Inference - Participants who scored high on this factor likely had a note-taking method that made later retrieval easier and more efficient.
