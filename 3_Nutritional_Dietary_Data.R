library(tidyverse)
library(ggplot2)
library(psych)
library(corrplot)

data <- read.csv("C:/Users/nicho/Downloads/3_Nutritional_Dietary_data_Group_007.csv", stringsAsFactors = FALSE)

str(data)
summary(data)

# Descriptive statistics for numeric columns
describe(data[,-1])

# Histograms 
data %>% 
  select(-Patient.ID) %>% 
  gather(key="Variable", value="Value") %>%
  ggplot(aes(x=Value)) +
  geom_histogram(fill="orchid4", bins=30) +
  facet_wrap(~Variable, scales="free") +
  theme_minimal()

# Scatter plot: Caloric intake vs Physical activity 
ggplot(data, aes(x=Physical_Activity_Hours_Week, y=Daily_Caloric_Intake_kcal)) +
  geom_point(color="darkmagenta") +
  geom_smooth(method="lm", se=FALSE, color="darkgreen") +
  labs(title="Calories vs Physical Activity", x="Activity (hours/week)", y="Calories/day")

# Bar graph: Mean intake per nutrient  
data %>%
  summarise(across(c(Protein_intake_g, Fat_intake_g, Carbohydrate_intake_g), mean)) %>%
  gather(key="Nutrient", value="Mean_Intake") %>%
  ggplot(aes(x=Nutrient, y=Mean_Intake, fill=Nutrient)) +
  geom_col() +
  scale_fill_manual(values=c("goldenrod", "darkorchid", "darkseagreen")) +
  theme_minimal() +
  labs(title="Mean Daily Intake per Nutrient")

# Correlation analysis — keep default corrplot style (already colorful)
corr_matrix <- cor(data[,-1])  
corrplot(corr_matrix, method="color", type="upper", tl.cex=0.😎

# Extract strong correlations (>0.7 or <-0.7)
strong_corrs <- which(abs(corr_matrix) > 0.7 & abs(corr_matrix) < 1, arr.ind = TRUE)
corr_matrix[strong_corrs]
