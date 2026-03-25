# ============================================
# A. PREPROCESSING PIPELINE
# ============================================

rm(list=ls())
library(dplyr)
library(caret)
library(smotefamily)

# Load data
dataset <- read.csv("train.csv")

# ----------------------------
# Feature Engineering
# ----------------------------

dataset$overworked <- ifelse(dataset$hours.per.week > 40,
                             "Overworked", "Not Overworked")

dataset$overworked <- as.factor(dataset$overworked)

dataset$age_group <- cut(dataset$age,
                         breaks = c(0,25,40,60,100),
                         labels = c("Young","Adult","Middle","Senior"))

dataset$capital_total <- dataset$capital.gain - dataset$capital.loss

dataset <- dataset %>% select(-fnlwgt, -hours.per.week)

dataset <- na.omit(dataset)

# ----------------------------
# Train/Test Split
# ----------------------------

set.seed(123)
train_index <- sample(1:nrow(dataset), 0.8*nrow(dataset))

train <- dataset[train_index, ]
test  <- dataset[-train_index, ]

# ----------------------------
# Encoding + Scaling
# ----------------------------

dummies <- dummyVars(overworked ~ ., data = train)

x_train <- predict(dummies, newdata = train)
x_test  <- predict(dummies, newdata = test)

x_train <- scale(x_train)
x_test  <- scale(x_test)

y_train <- train$overworked
y_test  <- test$overworked

# ----------------------------
# SMOTE Balancing
# ----------------------------

smote_data <- SMOTE(x_train, y_train, K = 5)

x_train <- smote_data$data[, -ncol(smote_data$data)]
y_train <- smote_data$data[, ncol(smote_data$data)]

# Convert labels
y_train_num <- ifelse(y_train == "Overworked", 1, 0)
y_test_num  <- ifelse(y_test == "Overworked", 1, 0)