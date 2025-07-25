# ----------------
# Load Libraries
# ----------------

install.packages("tidyverse")
install.packages("psych")
install.packages("GGally")
install.packages("corrplot")

library(tidyverse)
library(psych)
library(GGally)
library(corrplot)

# -------------------
# Load the data set
# -------------------

df <- read.csv("C:/Users/nicho/Downloads/1_Vital_signs_diagnosis_data_Group_007.csv", stringsAsFactors = FALSE)

# ----------------------
# Clean and Preprocess
# ----------------------

# Convert to numeric
df$Systolic_BP <- as.numeric(gsub("[^0-9.]", "", df$Systolic_BP))
df$Diastolic_BP <- as.numeric(gsub("[^0-9.]", "", df$Diastolic_BP))
df$Hypertension <- as.numeric(gsub("[^0-9.]", "", df$Hypertension))

# Factor conversion for categorical variables
df$Sex <- factor(df$Sex, levels = c(0, 1), labels = c("Female", "Male"))
df$Smoking_Status <- factor(df$Smoking_Status,
                            levels = c(0, 1, 2),
                            labels = c("Non-Smoker", "Occasional", "Chainsmoker"))

# Drop legend columns
df <- df %>% select(-contains("Legend"))

# Remove implausible BMI (zero or below)
df <- df %>% filter(BMI > 10)

# --------------------------
# Outlier Removal Function
# --------------------------
remove_outliers <- function(df, col) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  df %>% filter(df[[col]] >= (Q1 - 1.5 * IQR) & df[[col]] <= (Q3 + 1.5 * IQR))
}

# Apply outlier removal to selected columns
df_clean <- df %>%
  remove_outliers("Glucose_mg.dL") %>%
  remove_outliers("Cholesterol_mg.dL") %>%
  remove_outliers("BMI")

# -----------------------
# Descriptive Statistics
# -----------------------
summary(df_clean)
describe(df_clean %>% select(where(is.numeric)))

# ---------------
# Visualization 
# ---------------

# Histograms by Sex
histogram_vars <- c("BMI", "Glucose_mg.dL", "Cholesterol_mg.dL")
for (col in histogram_vars) {
  print(
    ggplot(df_clean %>% filter(!is.na(.data[[col]])), aes_string(x = col, fill = "Sex")) +
      geom_histogram(color = "black", bins = 30, position = "identity", alpha = 0.6) +
      facet_wrap(~ Sex) +
      labs(title = paste("Distribution of", col, "by Sex"),
           x = col, fill = "Sex") +
      theme_minimal()
  )
}

# Bar plot for Smoking_Status
ggplot(df_clean %>% filter(!is.na(Smoking_Status)), aes(x = Smoking_Status)) +
  geom_bar(fill = "mediumpurple", color = "black") +
  labs(title = "Smoking Status Frequency",
       x = "Smoking Status", y = "Count") +
  theme_minimal()

# Box Plots by Sex
boxplot_vars <- c("BMI", "Glucose_mg.dL", "Cholesterol_mg.dL")
for (col in boxplot_vars) {
  print(
    ggplot(df_clean %>% filter(!is.na(.data[[col]])), aes_string(x = "Sex", y = col, fill = "Sex")) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste("Boxplot of", col, "by Sex"),
           x = "Sex", y = col, fill = "Sex") +
      theme_minimal()
  )
}

# Scatter Plots: BMI vs other continuous variables
scatter_vars <- c("Systolic_BP", "Diastolic_BP", "Glucose_mg.dL", "Cholesterol_mg.dL")
for (col in scatter_vars) {
  print(
    ggplot(df_clean %>% filter(!is.na(BMI), !is.na(.data[[col]])), aes_string(x = "BMI", y = col, color = "Sex")) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
      labs(title = paste("Scatterplot of BMI vs", col),
           x = "BMI", y = col, color = "Sex") +
      theme_minimal()
  )
}

# --------------------
# Statistical Tests 
# --------------------

# t-test: Glucose by Sex
t_test_result <- t.test(Glucose_mg.dL ~ Sex, data = df_clean)
print(t_test_result)

# Correlation matrix (rounded for clarity)
cor_matrix <- cor(df_clean %>% select(where(is.numeric)), use = "complete.obs")
print(round(cor_matrix, 2))

# Visual correlation matrix
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

# --------------------
# Save cleaned data 
# --------------------

#save
write.csv(df_clean, "cleaned_Vital_signs_diagnosis_data.csv", row.names = FALSE)
