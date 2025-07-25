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

df <- read.csv("C:/Users/nicho/Downloads/3_Nutritional_Dietary_data_Group_007.csv", stringsAsFactors = FALSE)

# ----------------------
# Clean and Preprocess
# ----------------------

names(df)

# Convert numeric-like columns (remove non-numeric characters just in case)
df$Daily_Caloric_Intake_kcal <- as.numeric(gsub("[^0-9.]", "", df$Daily_Caloric_Intake_kcal))
df$Protein_intake_g <- as.numeric(gsub("[^0-9.]", "", df$Protein_intake_g))
df$Fat_intake_g <- as.numeric(gsub("[^0-9.]", "", df$Fat_intake_g))
df$Carbohydrate_intake_g <- as.numeric(gsub("[^0-9.]", "", df$Carbohydrate_intake_g))
df$Water_intake_ml <- as.numeric(gsub("[^0-9.]", "", df$Water_intake_ml))
df$Vitamin_C_mg <- as.numeric(gsub("[^0-9.]", "", df$Vitamin_C_mg))
df$Iron_mg <- as.numeric(gsub("[^0-9.]", "", df$Iron_mg))
df$Body_Fat_percent <- as.numeric(gsub("[^0-9.]", "", df$Body_Fat_percent))
df$Muscle_Mass_kg <- as.numeric(gsub("[^0-9.]", "", df$Muscle_Mass_kg))
df$BMI <- as.numeric(gsub("[^0-9.]", "", df$BMI))
df$Physical_Activity_Hours_Week <- as.numeric(gsub("[^0-9.]", "", df$Physical_Activity_Hours_Week))

# Optional: shorten variable names for plotting
df$Daily_Calories <- df$Daily_Caloric_Intake_kcal
df$Protein <- df$Protein_intake_g
df$Fat <- df$Fat_intake_g
df$Carbs <- df$Carbohydrate_intake_g
df$Water_L <- df$Water_intake_ml / 1000  # Convert to liters

# --------------------------
# Outlier Removal Function
# --------------------------
remove_outliers <- function(df, col) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  df %>% filter(df[[col]] >= (Q1 - 1.5 * IQR) & df[[col]] <= (Q3 + 1.5 * IQR))
}

# Apply outlier removal to key continuous variables
df_clean <- df %>%
  remove_outliers("Daily_Calories") %>%
  remove_outliers("Protein") %>%
  remove_outliers("Fat") %>%
  remove_outliers("Carbs") %>%
  remove_outliers("Water_L") %>%
  remove_outliers("BMI") %>%
  remove_outliers("Muscle_Mass_kg") %>%
  remove_outliers("Body_Fat_percent")

# -----------------------
# Descriptive Statistics
# -----------------------
summary(df_clean)
describe(df_clean %>% select(where(is.numeric)))

# ---------------
# Visualization 
# ---------------

# Histograms of key nutrient variables
histogram_vars <- c("Daily_Calories", "Protein", "Fat", "Carbs")
for (col in histogram_vars) {
  print(
    ggplot(df_clean %>% filter(!is.na(.data[[col]])), aes_string(x = col)) +
      geom_histogram(color = "black", fill = "skyblue", bins = 30) +
      labs(title = paste("Distribution of", col),
           x = col, y = "Count") +
      theme_minimal()
  )
}

# Boxplots for each nutrient
boxplot_vars <- c("Daily_Calories", "Protein", "Fat", "Carbs")
for (col in boxplot_vars) {
  print(
    ggplot(df_clean, aes_string(y = col)) +
      geom_boxplot(fill = "lightgreen", alpha = 0.7, outlier.color = "red") +
      labs(title = paste("Boxplot of", col),
           y = col) +
      theme_minimal()
  )
}

nutrient_data <- df_clean %>%
  select(Daily_Calories, Protein, Fat, Carbs) %>%
  pivot_longer(cols = everything(), names_to = "Nutrient", values_to = "Value")

# Scatterplots: Daily Calories vs Macronutrients
scatter_vars <- c("Protein", "Fat", "Carbs", "Water_L")
for (col in scatter_vars) {
  print(
    ggplot(df_clean, aes_string(x = col, y = "Daily_Calories")) +
      geom_point(alpha = 0.6, color = "tomato") +
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      labs(title = paste("Scatterplot of", col, "vs Daily Calories"),
           x = col, y = "Daily Calories") +
      theme_minimal()
  )
}

# --------------------
# Statistical Tests 
# --------------------

# Correlation matrix
cor_matrix <- cor(df_clean %>% select(where(is.numeric)), use = "complete.obs")
print(round(cor_matrix, 2))

# Visual correlation matrix
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

# --------------------
# Save cleaned data 
# --------------------

write.csv(df_clean, "C:/Users/nicho/Downloads/cleaned_Nutritional_Dietary_data.csv", row.names = FALSE)
