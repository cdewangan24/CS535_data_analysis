# Load Data
data <- data_R

# 1. Remove metadata columns
survey_items <- data[, !(names(data) %in% c("Participant", "NoteTakingTime", "RetrievalTime", "Preference"))]

# 2. Convert all to numeric
survey_items[] <- lapply(survey_items, function(x) as.numeric(as.character(x)))

# 3. Drop columns with any NA (due to failed conversion or empty cells)
survey_items <- survey_items[, colSums(is.na(survey_items)) == 0]

# 4. Drop columns with no variance
survey_items <- survey_items[, apply(survey_items, 2, function(x) var(x, na.rm = TRUE) > 0)]

# Install and load necessary packages
if (!require("psych")) install.packages("psych")
if (!require("GPArotation")) install.packages("GPArotation")
library(psych)
library(GPArotation)

# 5. KMO Test
kmo_result <- KMO(survey_items)
print(kmo_result)

# 6. Scree Plot (Parallel Analysis)
fa.parallel(survey_items, fa = "fa", fm = "pa", n.obs = nrow(survey_items))

# 7. Run EFA (forcing 1-factor solution based on scree)
efa_result <- fa(survey_items, nfactors = 1, rotate = "varimax", fm = "pa")

# 8. Print loadings (cutoff = 0.3)
print(efa_result$loadings, cutoff = 0.3)

# 9. View communalities
print(efa_result$communality)

# 10. Extract loading matrix
loading_matrix <- as.data.frame(unclass(efa_result$loadings))
loading_matrix$Item <- rownames(loading_matrix)

# 11. Filter meaningful items (≥ 0.3 loading)
retained_items <- loading_matrix[abs(loading_matrix$PA1) >= 0.3, c("Item", "PA1")]
print(retained_items)

# 12. Show original item names
print(colnames(survey_items)[as.integer(retained_items$Item)])

#Extracted one meaningful factor, which we can label as “Retrieval Effectiveness.” This factor:

#Explains ~25% of the variance in the data.

#Is primarily represented by these items:
  
#Retrieval2: Strong loading (0.81)

#Retrieval3: Moderate loading (0.59)

#General2: Moderate loading (0.57)

#Materiality1: Lower but still meaningful (0.38)

# Inference - Participants who scored high on this factor likely had a note-taking method that made later retrieval easier and more efficient.
