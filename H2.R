library(dplyr)
library(lubridate)

#Load CSV
data <- read.csv("data_R.csv")

data[, c("Retrieval1", "Retrieval2", "Retrieval3", "Retrieval4", "Retrieval5", "Retrieval6", "Retrieval7")]

#average of Retrieval1 to Retrieval7
data <- data %>%
  mutate(
    RetrievalScore = rowMeans(data[, c("Retrieval1", "Retrieval2", "Retrieval3", "Retrieval4", "Retrieval5", "Retrieval6", "Retrieval7")], na.rm = TRUE),
    Preference = factor(Preference, levels = c(1, 2), labels = c("Digital", "Physical"))
  )


#Filtering missing values
filtered_data <- data %>%
  filter(!is.na(Preference) & !is.na(RetrievalScore))

#Normality checking
shapiro.test(filtered_data$RetrievalScore)



#T test 
cat("\n== Retrieval Score T-Test (Physical < Digital) ==\n")
print(t.test(RetrievalScore ~ Preference,
             data = filtered_data,
             alternative = "less",   # "less" because your hypothesis is Physical < Digital
             var.equal = FALSE))
#t test has the p value as 0.418 implying its not significant as p?0.05 is not significant 

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

