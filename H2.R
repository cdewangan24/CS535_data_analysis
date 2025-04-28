library(dplyr)
library(lubridate)

#Load CSV
data <- read.csv("data_R.csv")

data[, c("Retrieval1", "Retrieval2", "Retrieval3", "Retrieval4", "Retrieval5", "Retrieval7", "Retrieval8")]

#average of Retrieval1 to Retrieval7
data <- data %>%
  mutate(
    RetrievalScore = rowMeans(data[, c("Retrieval1", "Retrieval2", "Retrieval3", "Retrieval4", "Retrieval5", "Retrieval7", "Retrieval8")], na.rm = TRUE),
    Preference = factor(Preference, levels = c(1, 2), labels = c("Physical", "Digital"))
  )


#Filtering missing values
filtered_data <- data %>%
  filter(!is.na(Preference) & !is.na(RetrievalScore))

#Normality checking
shapiro.test(filtered_data$RetrievalScore)

# Wilcoxon tests
cat("\n== Retrieval Score Test (Physical < Digital) ==\n")
print(wilcox.test(RetrievalScore ~ Preference,
                  data = filtered_data,
                  alternative = "less",
                  exact = FALSE))

# Descriptive stats
filtered_data %>%
  group_by(Preference) %>%
  summarise(
    Count = n(),
    Mean_RetrievalScore = mean(RetrievalScore),
    Median_RetrievalScore = median(RetrievalScore),
    SD_RetrievalScore = sd(RetrievalScore)
  )


par(mfrow = c(1, 2))

boxplot(RetrievalScore ~ Preference, data = filtered_data,
        main = "Self-Perceived Retrieval Efficiency",
        col = c("skyblue", "lightgreen"))
par(mfrow = c(1,1))

