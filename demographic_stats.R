# 1. Load required packages
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# 2. Load the dataset
df <- demographics_data

# 3. Basic Summaries

# Summary of Age
print("Summary of Age:")
print(summary(df$Age))

# Count tables
print("Gender Count:")
print(table(df$Gender))

print("Academic Level Count:")
print(table(df$Academic_Level))

print("Field of Study Count:")
print(table(df$Field_of_Study))

print("Note-Taking Preference Count:")
print(table(df$Preference))

print("Frequency of Note-Taking Count:")
print(table(df$Frequency))

print("Reasons for Preference Count:")
print(table(df$Why_Preference))

# 4. Bar Plots

# Gender
ggplot(df, aes(x = Gender)) +
  geom_bar(fill = "steelblue") +
  ggtitle("Gender Distribution")

# Academic Level
ggplot(df, aes(x = Academic_Level)) +
  geom_bar(fill = "darkgreen") +
  ggtitle("Academic Level Distribution")

# Field of Study
ggplot(df, aes(x = Field_of_Study)) +
  geom_bar(fill = "orange") +
  ggtitle("Field of Study Distribution")

# Preference
ggplot(df, aes(x = Preference)) +
  geom_bar(fill = "purple") +
  ggtitle("Note-Taking Preference")

# Frequency of Note-Taking
ggplot(df, aes(x = Frequency)) +
  geom_bar(fill = "tomato") +
  ggtitle("Frequency of Note-Taking")

# Reasons for Preference
ggplot(df, aes(x = Why_Preference)) +
  geom_bar(fill = "gold") +
  ggtitle("Reasons for Preference") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 5. Group Comparisons

# Mean age by preference
print("Mean Age by Note-Taking Preference:")
print(aggregate(Age ~ Preference, data = df, FUN = mean))

# Gender vs Preference
print("Gender vs Preference:")
print(table(df$Gender, df$Preference))

# Academic Level vs Preference
print("Academic Level vs Preference:")
print(table(df$Academic_Level, df$Preference))

# Frequency by Preference
print("Note-Taking Frequency by Preference:")
print(table(df$Preference, df$Frequency))

#Inferences:-
#Mean and median age: 22 years

#Age range: 20–24 years, all young adults

#4 females, 2 males

#All physical note-takers are female

#Even split between graduate and undergraduate students (3 each)

#5 participants are from Computer Science, 1 from Medicine

#4 participants prefer digital note-taking, 2 prefer physical

#3 participants take notes weekly, 2 daily, 1 occasionally

#Digital note-taking is preferred for ease of use (2), organization (2), and convenience (1)

#Physical note-taking is preferred for cognitive retention (1)

#All male participants prefer digital note-taking

#Physical note-takers are slightly younger (average age 21) compared to digital users (average age 22.5)

#Preference patterns appear linked to gender, field of study, and perceived functional benefit