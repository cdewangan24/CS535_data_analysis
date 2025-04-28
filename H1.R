library(tidyverse)
library(dplyr)

# Loading csv
df<-read.csv("data_R.csv")

# HH:MM:SS to seconds function
time_to_seconds <- function(time_str) {
  parts <- strsplit(as.character(time_str), ":")[[1]]
  if (length(parts) == 3) {
    return(as.numeric(parts[1])*3600 + as.numeric(parts[2])*60 + as.numeric(parts[3]))
  } else {
    return(NA)
  }
}

df$NoteTakingTime_sec <- sapply(df$NoteTakingTime, time_to_seconds)
df$RetrievalTime_sec <- sapply(df$RetrievalTime, time_to_seconds)

# 1 - Digital and 2 - Physical
df$Preference <- factor(df$Preference, levels = c("1", "2"))
df$Preference

"-----------------------------------------------------------------------------------------------"
"Hypothesis 1 - Physical note-taking participants will take significantly more time 
for both note-taking and retrieval tasks than digital note-taking participants.
Since both the time were not normal we can use."

# First we will do normality tests on Note-Taking Time and Retrieval Time
# Note-Taking Time
shapiro.test(df$NoteTakingTime_sec)
# NoteTakingTime_sec - Resulted in not normal 

# Retrieval Time
shapiro.test(df$RetrievalTime_sec)
# RetrievalTime_sec - Resulted in not normal 

# Using non-parametric tests
# Note-Taking Time: Physical vs Digital
wilcox.test(NoteTakingTime_sec ~ Preference, data = df)

# Retrieval Time: Physical vs Digital
wilcox.test(RetrievalTime_sec ~ Preference, data = df)

# The test revealed no significant differences in:
# Note-taking time (W = 2, p = 0.53)
# Retrieval time (W = 5, p = 0.80)

# Since inferential tests doesn't give much insights, we will now use descriptive stats

df %>%
  group_by(Preference) %>%
  summarise(
    Mean_Note = mean(NoteTakingTime_sec, na.rm = TRUE),
    Median_Note = median(NoteTakingTime_sec, na.rm = TRUE),
    SD_Note = sd(NoteTakingTime_sec, na.rm = TRUE),
    Mean_Retrieval = mean(RetrievalTime_sec, na.rm = TRUE),
    Median_Retrieval = median(RetrievalTime_sec, na.rm = TRUE),
    SD_Retrieval = sd(RetrievalTime_sec, na.rm = TRUE),
    N = n()
  )

"On average, digital note-takers (n = 4) spent 1,892 seconds on note-taking (SD = 2,384), while 
physical note-takers (n = 2) spent 1,960 seconds (SD = 713). Despite a slightly higher mean for 
the physical group, the digital group had a much lower median (777 sec), indicating high 
variability — likely driven by one or more long note-taking sessions."

boxplot(NoteTakingTime_sec ~ Preference, data = df,
        main = "Note-Taking Time by Method", ylab = "Seconds", col = c("skyblue", "lightgreen"))

boxplot(RetrievalTime_sec ~ Preference, data = df,
        main = "Retrieval Time by Method", ylab = "Seconds", col = c("skyblue", "lightgreen"))

# The above results can be also be verified by looking at the box plots and see the high variability.
