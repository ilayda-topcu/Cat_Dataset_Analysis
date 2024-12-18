# Load necessary libraries
library(ggplot2)    # For visualizations
library(dplyr)      # For data manipulation
library(readr)      # For reading in the data

# Load the data
cat_dataset <-read.csv("C:/Users/ASUS/Downloads/cats_dataset.csv", header=TRUE, sep=";",encoding="UTF-8", check.names = FALSE, stringsAsFactors = FALSE) # Read .csv file

colnames(cats_dataset) <- c("breed", "age(years)","weight(kg)", "color", "gender")
head(cats_dataset)

# Check the structure of the data
str(cats_dataset)

# View the first few rows
head(cats_dataset)

# Summary statistics for numeric columns (Age and Weight)
summary(cats_dataset)

# Distribution of breed
ggplot(cats_dataset, aes(x = breed)) + 
  geom_bar(fill = "#06bfc4") + 
  labs(title = "Distribution of breed", x = "Breed", y = "Frequency") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1))

# Color distribution
ggplot(cats_dataset, aes(x = color,)) + 
  geom_bar(fill = "#e06bc9") + 
  labs(title = "Color distribution", x = "Color", y = "Frequency") + 
  theme(axis.text.x = element_text(angle = 50, hjust = 1))


# Histogram of Age
ggplot(cats_dataset, aes(x = `age(years)`)) +
  geom_histogram(binwidth = 1, fill = "#e69f01", color = "black") +
  labs(title = "Age Distribution of Cats", x = "Age (Years)", y = "Frequency")

# Histogram of Weight
ggplot(cats_dataset, aes(x = `weight(kg)`)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  labs(title = "Weight Distribution of Cats", x = "Weight (kg)", y = "Frequency")


# Boxplot for Age by Gender
ggplot(cats_dataset, aes(x = gender, y = `age(years)`, fill = gender)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Gender", x = "Gender", y = "Age (Years)")

# Boxplot for Weight by Gender
ggplot(cats_dataset, aes(x = gender, y = `weight(kg)`, fill = gender)) +
  geom_boxplot() +
  labs(title = "Weight Distribution by Gender", x = "Gender", y = "Weight (kg)")

# Count plot for Color by Gender
ggplot(cats_dataset, aes(x = color, fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Color and Gender Relationship", x = "Color", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot for Weight by Breed
ggplot(cats_dataset, aes(y = breed, x = `weight(kg)`, fill = breed)) +
  geom_boxplot() +
  labs(title = "Weight Distribution by Breed", y = "Breed", x = "Weight (kg)") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

# 1. Color and Gender Relationship
# Is there a relationship between the color and gender of cats?

# Contingency table for Chi-Square test
color_gender_table <- table(cats_dataset$color, cats_dataset$gender)

# Chi-Square test
chi_test <- chisq.test(color_gender_table)
chi_test

# Since the p-value of this test is 0.03282, at the generally accepted 5% significance level (α = 0.05), the following conclusion is reached: - H₀ is rejected.
# It is concluded that there is a significant relationship between the color of the cats and their gender.

# Additional Analysis
chi_test$expected

ggplot(cats_dataset, aes(x = color, fill = gender)) + 
  geom_bar(position = "fill") +
  labs(title = "Color and Gender Relationship", x = "Color", y = "Ratio") +
  theme_minimal()

chi_test$residuals


# 2. Breed and Weight Distribution
# How does the weight distribution of different cat breeds differ?

# ANOVA
anova_model <- aov(`weight(kg)` ~ breed, data = cats_dataset)

summary(anova_model)

# 3. Predicting Gender by Age and Weight 
# Can we predict the gender of cats based on their age and weight?

# Logistic regression model
cats_dataset$gender <- ifelse(cats_dataset$gender == "Female", 1, 0)
logistic_model <- glm(gender ~ `age(years)` + `weight(kg)`, 
                      data = cats_dataset, 
                      family = "binomial")

summary(logistic_model)

