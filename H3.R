if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
# Hypothesis 3 - Physical note-taking participants will report higher levels of concern related to sustainability.

df<-read.csv("data_R.csv")
# Convert Preference to labeled factor
df$Preference <- factor(df$Preference, levels = c("1", "2"), labels = c("Digital", "Physical"))

# Filter out missing Sustainability data
sustain_data <- df %>%
  filter(!is.na(Sustainability1))

#Normality test
shapiro.test(sustain_data$Sustainability1)

# Non-parametric test
wilcox.test(Sustainability1 ~ Preference, data = sustain_data)

#Descriptive Stats
sustain_data %>%
  group_by(Preference) %>%
  summarise(
    Mean = mean(Sustainability1, na.rm = TRUE),
    Median = median(Sustainability1, na.rm = TRUE),
    SD = sd(Sustainability1, na.rm = TRUE),
    N = n()
  )

#Box Plot
boxplot(Sustainability1 ~ Preference, data = sustain_data,
        main = "Sustainability Concern by Note-Taking Method",
        ylab = "Sustainability Concern",
        col = c("skyblue", "lightgreen"))



