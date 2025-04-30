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

#An exploratory factor analysis (EFA) was conducted using principal axis factoring with varimax rotation on six survey items to examine the underlying latent structure. Sampling adequacy was assessed using the Kaiser-Meyer-Olkin (KMO) measure, which yielded an overall MSA of 0.60—meeting the minimum acceptable threshold for factor analysis. A parallel analysis and scree plot both suggested a one-factor solution.

#The resulting factor accounted for approximately 25.2% of the total variance, indicating modest explanatory power. Three items exhibited meaningful loadings above the .30 threshold, with one item demonstrating a particularly strong loading of .811, suggesting it is a central indicator of the latent construct. Communality estimates further supported this structure, with the strongest item explaining 66% of its variance through the factor. In contrast, several other items displayed weak or negligible loadings and communalities, suggesting limited alignment with the underlying construct.

#Taken together, these results provide preliminary support for a single latent factor—tentatively interpretable as Retrieval Effectiveness—driven primarily by a subset of items. However, given the small sample size and low communalities for some items, these findings should be interpreted with caution and warrant validation in future studies with larger samples.


