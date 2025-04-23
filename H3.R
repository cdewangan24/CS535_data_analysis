if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

note_data <- `Final_Data_R`

note_data$Group <- ifelse(note_data$Preference == 1, "Digital", "Physical")

sustainability_data <- note_data %>%
  filter(!is.na(Sustainability2)) %>%
  select(Group, Sustainability2)

sustainability_stats <- sustainability_data %>%
  group_by(Group) %>%
  summarise(
    Count = n(),
    Mean = mean(Sustainability2),
    Median = median(Sustainability2),
    SD = sd(Sustainability2),
    Min = min(Sustainability2),
    Max = max(Sustainability2)
  )
print(sustainability_stats)

test_result <- wilcox.test(Sustainability2 ~ Group, data = sustainability_data, exact = FALSE)
print(test_result)

boxplot(Sustainability2 ~ Group, data = sustainability_data,
        main = "Sustainability Concern by Note-Taking Group",
        ylab = "Self-Reported Sustainability Concern",
        xlab = "Note-Taking Group",
        col = c("skyblue", "lightgreen"))

n1 <- sum(sustainability_data$Group == "Digital")
n2 <- sum(sustainability_data$Group == "Physical")

U <- test_result$statistic

r <- as.numeric(U) / (n1 * n2)
cat("Effect size r =", round(r, 3))

