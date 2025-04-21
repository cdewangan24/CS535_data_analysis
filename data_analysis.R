library(tidyverse)

df<-read.csv("total_data.csv")

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

df$Participant

df$Preference <- factor(df$Preference, levels = c("Digital", "Physical"))

# Note-Taking Time
shapiro.test(df$NoteTakingTime_sec)

# Retrieval Time
shapiro.test(df$RetrievalTime_sec)

# Hypothesis 1

# Hypothesis 2

# Hypothesis 3
