# ============================================================
# YuvaIntern - Virtual R Data Analyst Internship
# Week 1: Data Cleaning and Preliminary Analysis with R
# Author: Pradnya Patil
# Dataset: Titanic Passenger Dataset
# ============================================================

# -----------------------------
# 1. Install / load packages
# -----------------------------
# Run this once if tidyverse is not installed:
# install.packages("tidyverse")

library(tidyverse)

# -----------------------------
# 2. Download / load dataset
# -----------------------------
# If titanic.csv is already in this folder, it will be used.
# Otherwise, the script downloads the publicly available dataset.

file_name <- "titanic.csv"
dataset_url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"

if (!file.exists(file_name)) {
  download.file(dataset_url, destfile = file_name, mode = "wb")
}

titanic <- read.csv(file_name, stringsAsFactors = FALSE)

# -----------------------------
# 3. Initial inspection
# -----------------------------
cat("\n===== FIRST 6 ROWS =====\n")
print(head(titanic))

cat("\n===== STRUCTURE =====\n")
str(titanic)

cat("\n===== SUMMARY =====\n")
print(summary(titanic))

cat("\n===== DIMENSIONS =====\n")
print(dim(titanic))

cat("\n===== COLUMN NAMES =====\n")
print(names(titanic))

# -----------------------------
# 4. Missing-value analysis
# -----------------------------
cat("\n===== MISSING VALUES =====\n")
missing_count <- colSums(is.na(titanic))
print(missing_count)

cat("\n===== MISSING VALUE PERCENTAGE =====\n")
missing_percent <- colMeans(is.na(titanic)) * 100
print(round(missing_percent, 2))

# -----------------------------
# 5. Handle missing values
# -----------------------------
# Age: median imputation
titanic$Age[is.na(titanic$Age)] <-
  median(titanic$Age, na.rm = TRUE)

# Embarked: replace missing values with mode
mode_value <- names(sort(table(titanic$Embarked),
                         decreasing = TRUE))[1]

titanic$Embarked[is.na(titanic$Embarked)] <- mode_value

# Cabin has many missing values.
# Instead of inventing cabin numbers, create a useful indicator:
titanic$CabinAvailable <- ifelse(
  is.na(titanic$Cabin), "No", "Yes"
)

cat("\n===== MISSING VALUES AFTER CLEANING =====\n")
print(colSums(is.na(titanic)))

# -----------------------------
# 6. Categorical variables
# -----------------------------
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$CabinAvailable <- as.factor(titanic$CabinAvailable)

# Binary encoding example
titanic$Sex_Male <- ifelse(titanic$Sex == "male", 1, 0)

# -----------------------------
# 7. Outlier detection
# -----------------------------
cat("\n===== FARE OUTLIER LIMITS =====\n")

Q1 <- quantile(titanic$Fare, 0.25, na.rm = TRUE)
Q3 <- quantile(titanic$Fare, 0.75, na.rm = TRUE)
IQR_value <- IQR(titanic$Fare, na.rm = TRUE)

lower_limit <- Q1 - 1.5 * IQR_value
upper_limit <- Q3 + 1.5 * IQR_value

cat("Lower limit:", lower_limit, "\n")
cat("Upper limit:", upper_limit, "\n")

fare_outliers <- titanic[
  titanic$Fare < lower_limit |
  titanic$Fare > upper_limit,
]

cat("Number of potential Fare outliers:",
    nrow(fare_outliers), "\n")

# Visual inspection
boxplot(
  titanic$Age,
  main = "Boxplot of Passenger Age",
  ylab = "Age"
)

boxplot(
  titanic$Fare,
  main = "Boxplot of Passenger Fare",
  ylab = "Fare"
)

# -----------------------------
# 8. Normalization
# -----------------------------
min_max <- function(x) {
  (x - min(x, na.rm = TRUE)) /
    (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

titanic$Age_scaled <- min_max(titanic$Age)
titanic$Fare_scaled <- min_max(titanic$Fare)

# -----------------------------
# 9. Descriptive analysis
# -----------------------------
cat("\n===== SURVIVAL COUNTS =====\n")
print(table(titanic$Survived))

cat("\n===== SURVIVAL BY SEX =====\n")
print(table(titanic$Sex, titanic$Survived))

cat("\n===== SURVIVAL BY CLASS =====\n")
print(table(titanic$Pclass, titanic$Survived))

cat("\n===== NUMERICAL SUMMARY =====\n")
print(summary(
  titanic[, c("Age", "SibSp", "Parch", "Fare")]
))

# -----------------------------
# 10. Correlation analysis
# -----------------------------
numeric_data <- titanic[
  , c("Survived", "Age", "SibSp", "Parch", "Fare")
]

correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

cat("\n===== CORRELATION MATRIX =====\n")
print(round(correlation_matrix, 3))

# -----------------------------
# 11. Visualizations
# -----------------------------

# Age distribution
hist(
  titanic$Age,
  main = "Age Distribution",
  xlab = "Age",
  ylab = "Frequency",
  breaks = 20
)

# Survival count
barplot(
  table(titanic$Survived),
  main = "Survival Count",
  xlab = "Survived (0 = No, 1 = Yes)",
  ylab = "Number of Passengers"
)

# Fare by survival
boxplot(
  Fare ~ Survived,
  data = titanic,
  main = "Fare Distribution by Survival",
  xlab = "Survived (0 = No, 1 = Yes)",
  ylab = "Fare"
)

# Survival by sex
barplot(
  table(titanic$Sex, titanic$Survived),
  beside = TRUE,
  main = "Survival by Sex",
  xlab = "Survival",
  ylab = "Number of Passengers",
  legend.text = TRUE
)

# -----------------------------
# 12. Optional ggplot2 graphs
# -----------------------------

ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 20) +
  labs(
    title = "Passenger Age Distribution",
    x = "Age",
    y = "Count"
  )

ggplot(titanic, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Survival by Sex",
    x = "Sex",
    fill = "Survived"
  )

# -----------------------------
# 13. Save cleaned dataset
# -----------------------------
write.csv(
  titanic,
  "titanic_cleaned.csv",
  row.names = FALSE
)

cat("\n===== ANALYSIS COMPLETE =====\n")
cat("Cleaned dataset saved as: titanic_cleaned.csv\n")
cat("Take screenshots of the R console and plots for the report.\n")
