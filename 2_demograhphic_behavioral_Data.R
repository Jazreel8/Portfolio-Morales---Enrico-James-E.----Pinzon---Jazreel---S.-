install.packages("tidyverse")
install.packages("janitor")
install.packages("GGally")
install.packages("corrplot")
install.packages("ggplot2")

# =======================
# Load Libraries
# =======================
library(tidyverse)
library(janitor)
library(psych)
library(ggplot2)
library(corrplot)   # For classic correlation plot

# ===========================
# Load and Clean Data set
# ===========================
df <- read.csv("C:/Users/nicho/Downloads/2_Demographic_Behavioral_data_Group_007.csv", stringsAsFactors = FALSE)

# Clean column names
df <- clean_names(df)

# Remove columns that are mostly empty (e.g., >80% NA)
df <- df[, colMeans(is.na(df)) < 0.8]

# Remove rows with NA in key variables
df_clean <- df %>%
  drop_na(age, sex, bmi, physical_activity_hours_week, smoking_status)

# Convert categorical variables to factors
df_clean <- df_clean %>%
  mutate(
    sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male")),
    smoking_status = factor(smoking_status, levels = c(0, 1, 2),
                            labels = c("Non-Smoker", "Occasional", "Chainsmoker"))
  )

# ==============================
# Descriptive Statistics
# ==============================

# Summary of numeric variables
summary(df_clean)
describe(df_clean %>% select(where(is.numeric)))

# =====================
# Data Visualizations
# =====================

# Histogram: Health Literacy Score by Sex
ggplot(df_clean, aes(x = health_literacy_score, fill = sex)) +
  geom_histogram(color = "black", bins = 9, alpha = 0.6, position = "identity") +
  facet_wrap(~ sex) +
  labs(title = "Health Literacy Score Distribution by Sex", x = "Health Literacy Score", fill = "Sex") +
  theme_minimal()

# Bar Plot: Smoking Status
ggplot(df_clean, aes(x = smoking_status)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(title = "Smoking Status Frequency", x = "Smoking Status", y = "Count") +
  theme_minimal()

# Box plot: BMI by Sex
ggplot(df_clean, aes(x = sex, y = bmi, fill = sex)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "BMI by Sex", x = "Sex", y = "BMI") +
  theme_minimal()

# Scatter Plot: BMI vs Physical Activity
ggplot(df_clean, aes(x = physical_activity_hours_week, y = bmi, color = sex)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  labs(title = "BMI vs Physical Activity", x = "Physical Activity (hrs/week)", y = "BMI") +
  theme_minimal()

# ===================================
# Statistical Analysis
# ===================================

# Correlation: BMI and Physical Activity
cor_test <- cor.test(df_clean$bmi, df_clean$physical_activity_hours_week)
print(cor_test)

# t-test: BMI by Sex
t_test <- t.test(bmi ~ sex, data = df_clean)
print(t_test)

# Correlation Matrix
cor_matrix <- cor(df_clean %>% select(where(is.numeric)), use = "complete.obs")
print(round(cor_matrix, 2))

# Visualize Correlation Matrix
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

# ===================================
# Save the cleaned data 
# ===================================
write.csv(df_clean, "cleaned_demographic_behavioral_data.csv", row.names = FALSE)
