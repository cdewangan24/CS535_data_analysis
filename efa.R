# 1. Load dataset
data <- data_R

# 2. Select only survey items
survey_items <- data[, !(names(data) %in% c("Participant", "NoteTakingTime", "RetrievalTime", "Preference"))]

# 3. Ensure all are numeric
survey_items[] <- lapply(survey_items, function(x) as.numeric(as.character(x)))

# 4. Transpose: 22 rows (questions) × 6 columns (participants)
survey_matrix <- t(as.matrix(survey_items))

# 5. Perform EFA
if (!require("psych")) install.packages("psych")
if (!require("GPArotation")) install.packages("GPArotation")
library(psych)
library(GPArotation)

# 6. Check suitability (KMO)
kmo_result <- KMO(survey_matrix)
print(kmo_result)

# 7. Scree Plot to decide number of factors
fa.parallel(survey_matrix, fa = "fa")

efa_result <- fa(survey_matrix, nfactors = 1, rotate = "varimax", fm = "pa")  # Principal Axis Factoring
print(efa_result$loadings, cutoff = 0.3)

# 10. View Communalities
print(efa_result$communality)
