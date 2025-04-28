library(tidyverse)
library(dplyr)

# Loading csv
df<-read.csv("data_R.csv")

# 1 - Digital and 2 - Physical
df$Preference <- factor(df$Preference, levels = c("1", "2"))
df$Preference

"-----------------------------------------------------------------------------------------------"

"Hypothesis 4 - Physical note-taking participants will demonstrate a significantly greater positive cognitive impact—measured through higher focus, lower cognitive load, and better retention—compared to digital note-taking participants."

df <- df %>%
  mutate(
    CognitiveScore = rowMeans(df[, c("Cognitive1", "Cognitive2", "Cognitive3", "Cognitive4", 
                                     "Cognitive5", "Cognitive6", "Cognitive7")], na.rm = TRUE),
    Preference = factor(Preference, levels = c("1", "2"), labels = c("Digital", "Physical"))
  )

# Filtering missing values
filtered_data <- df %>%
  filter(!is.na(Preference) & !is.na(CognitiveScore))

# Normality checking
shapiro.test(filtered_data$CognitiveScore)
# p-value was 0.6338 > 0.05 which means normal - parametric tests can be performed

# We can now do t-test
t.test(CognitiveScore ~ Preference, data = filtered_data)
# No significance difference was found that means fail to reject null hypothesis

# Since t-test showed no significant difference, now running descriptive statistics
filtered_data %>%
  group_by(Preference) %>%
  summarise(
    Mean = mean(CognitiveScore, na.rm = TRUE),
    Median = median(CognitiveScore, na.rm = TRUE),
    SD = sd(CognitiveScore, na.rm = TRUE),
    N = n()
  )
"Although the physical group showed greater variability, both groups had matching medians, 
suggesting no strong skew."

boxplot(CognitiveScore ~ Preference, data = filtered_data,
        main = "Cognitive Impact by Note-Taking Method",
        ylab = "Cognitive Score", col = c("skyblue", "lightgreen"))

"-----------------------------------------------------------------------------------------------"

