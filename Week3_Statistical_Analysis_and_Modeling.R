# ============================================================
# YuvaIntern - Week 3
# Statistical Analysis and Predictive Modeling using R
# ============================================================

library(tidyverse)

# Load data
titanic <- read.csv("titanic.csv", stringsAsFactors = FALSE)

# Prepare variables
titanic$Sex <- as.factor(titanic$Sex)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Embarked <- as.factor(titanic$Embarked)

titanic$Age[is.na(titanic$Age)] <-
  median(titanic$Age, na.rm = TRUE)

mode_embarked <- names(sort(table(titanic$Embarked),
                            decreasing = TRUE))[1]
titanic$Embarked[is.na(titanic$Embarked)] <- mode_embarked

# Select modeling variables
model_data <- titanic[, c(
  "Survived", "Pclass", "Sex", "Age",
  "SibSp", "Parch", "Fare", "Embarked"
)]

model_data <- na.omit(model_data)

# Hypothesis test
sex_survival_table <- table(
  model_data$Sex,
  model_data$Survived
)

chi_result <- chisq.test(sex_survival_table)
print(chi_result)

# Correlation
numeric_data <- model_data[, c(
  "Survived", "Age", "SibSp", "Parch", "Fare"
)]

correlation_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

print(round(correlation_matrix, 3))

# Train-test split
set.seed(123)

n <- nrow(model_data)

train_index <- sample(
  seq_len(n),
  size = floor(0.80 * n)
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Logistic regression
model <- glm(
  Survived ~ Pclass + Sex + Age +
    SibSp + Parch + Fare + Embarked,
  data = train_data,
  family = binomial
)

summary(model)

# Predictions
probabilities <- predict(
  model,
  newdata = test_data,
  type = "response"
)

predicted <- ifelse(probabilities >= 0.5, 1, 0)

# Confusion matrix
confusion_matrix <- table(
  Actual = test_data$Survived,
  Predicted = predicted
)

print(confusion_matrix)

# Metrics
cm <- confusion_matrix

TN <- cm["0", "0"]
FP <- cm["0", "1"]
FN <- cm["1", "0"]
TP <- cm["1", "1"]

accuracy <- (TP + TN) / sum(cm)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * precision * recall /
  (precision + recall)

cat("Accuracy :", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall   :", round(recall, 4), "\n")
cat("F1 Score :", round(f1_score, 4), "\n")

# ROC / AUC
# install.packages("pROC")
library(pROC)

roc_result <- roc(
  test_data$Survived,
  probabilities
)

plot(
  roc_result,
  main = "ROC Curve - Logistic Regression"
)

auc_value <- auc(roc_result)
print(auc_value)

# Model diagnostics
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Optional 5-fold cross-validation
# install.packages("caret")
library(caret)

set.seed(123)

control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE
)

cv_model <- train(
  factor(Survived) ~ Pclass + Sex + Age +
    SibSp + Parch + Fare + Embarked,
  data = model_data,
  method = "glm",
  family = binomial,
  trControl = control
)

print(cv_model)
