library(dplyr)
library(lubridate)

#Load CSV
data <- read.csv("Final_Data_R.csv")

#RetrievalTime to seconds
data$RetrievalTime <- sapply(data$RetrievalTime, function(x) {
  period <- hms(x)
  as.numeric(period, units = "secs")
})

#average of Retrieval1 to Retrieval6
data <- data %>%
  mutate(
    RetrievalScore = rowMeans(select(., Retrieval1:Retrieval6), na.rm = TRUE),
    Preference = factor(Preference, levels = c(1, 2), labels = c("Physical", "Digital"))
  )

#Filtering missing values
filtered_data <- data %>%
  filter(!is.na(Preference) & !is.na(RetrievalTime) & !is.na(RetrievalScore))

#Normality checking
shapiro.test(filtered_data$RetrievalTime)
shapiro.test(filtered_data$RetrievalScore)

# Wilcoxon tests
cat("== Retrieval Time Test (Physical > Digital) ==\n")
print(wilcox.test(RetrievalTime ~ Preference,
                  data = filtered_data,
                  alternative = "greater",
                  exact = FALSE))

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
    Mean_RetrievalTime = mean(RetrievalTime),
    Median_RetrievalTime = median(RetrievalTime),
    SD_RetrievalTime = sd(RetrievalTime),
    Mean_RetrievalScore = mean(RetrievalScore),
    Median_RetrievalScore = median(RetrievalScore),
    SD_RetrievalScore = sd(RetrievalScore)
  )


par(mfrow = c(1, 2))
boxplot(RetrievalTime ~ Preference, data = filtered_data,
        main = "Retrieval Time (in seconds)",
        col = c("skyblue", "lightgreen"))

boxplot(RetrievalScore ~ Preference, data = filtered_data,
        main = "Self-Perceived Retrieval Efficiency",
        col = c("skyblue", "lightgreen"))
par(mfrow = c(1,1))
