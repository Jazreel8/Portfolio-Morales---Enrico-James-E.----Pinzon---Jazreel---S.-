library(tidyverse)
library(psych)
library(ggplot2)

#Loading of data
data <- read.csv("C:/Users/nicho/Downloads/4_Time_series_Mointoring_data_Group_007.csv", stringsAsFactors = FALSE)

str(data)
summary(data)

# Descriptive statistics for steps, stress, and BMI
describe(data %>% select(contains("avg_steps"), contains("Stress"), contains("BMI")))

# Line plot of average steps per patient
data_long_steps <- data %>%
  select(Month_numerical, starts_with("Patient_")) %>%
  select(Month_numerical, contains("avg_steps")) %>%
  pivot_longer(cols = -Month_numerical, names_to = "Patient", values_to = "Steps")

ggplot(data_long_steps, aes(x = Month_numerical, y = Steps, color = Patient)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("darkorchid", "goldenrod", "forestgreen", "mediumvioletred", "darkolivegreen")) +
  labs(title="Monthly Step Count per Patient", x="Month", y="Average Steps") +
  theme_minimal()

# Line plot for stress levels
stress_data <- data %>%
  select(Month_numerical, contains("Stress")) %>%
  pivot_longer(-Month_numerical, names_to="Patient", values_to="Stress")

ggplot(stress_data, aes(x=Month_numerical, y=Stress, color=Patient)) +
  geom_line(size=1) +
  scale_color_manual(values = c("orchid4", "yellow3", "darkseagreen", "plum4", "mediumseagreen")) +
  labs(title="Stress Level Trends", x="Month", y="Stress Level") +
  theme_light()

# Boxplots for BMI
bmi_data <- data %>%
  select(contains("BMI")) %>%
  pivot_longer(everything(), names_to="Patient", values_to="BMI")

ggplot(bmi_data, aes(x=Patient, y=BMI, fill=Patient)) +
  geom_boxplot() +
  scale_fill_manual(values = c("darkorchid3", "khaki3", "darkolivegreen3", "orchid3", "mediumorchid")) +
  labs(title="BMI Distribution per Patient", x="", y="BMI") +
  theme_classic()

cor.test(data$Patient_1_avg_steps, data$Patient_1_Stress_Level)

lm_model <- lm(Patient_1_Stress_Level ~ Patient_1_avg_steps, data=data)
summary(lm_model)
dingdong morales
