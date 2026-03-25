library(ROCR)
library(caret)

evaluate_model <- function(pred_prob, pred_class, y_true_num, y_true_factor) {
  
  # Confusion Matrix
  cm <- confusionMatrix(as.factor(pred_class), as.factor(y_true_num))
  print(cm)
  
  # ROC + AUC
  pred <- prediction(pred_prob, y_true_num)
  perf <- performance(pred, "tpr", "fpr")
  plot(perf, col="blue", main="ROC Curve")
  abline(a=0,b=1,col="gray")
  
  auc <- performance(pred, "auc")@y.values[[1]]
  print(paste("AUC:", auc))
}

# ============================================
# Logistic Regression
# ============================================

log_model <- glm(overworked ~ ., data = train, family = binomial)

# Predictions
log_prob <- predict(log_model, test, type="response")
log_class <- ifelse(log_prob > 0.5, 1, 0)

evaluate_model(log_prob, log_class, y_test_num, y_test)

# ============================================
# XGBoost
# ============================================

library(xgboost)

xgb_model <- xgboost(
  data = as.matrix(x_train),
  label = y_train_num,
  nrounds = 200,
  max_depth = 3,
  eta = 0.1,
  objective = "binary:logistic",
  eval_metric = "auc"
)

# Predictions
xgb_prob <- predict(xgb_model, as.matrix(x_test))
xgb_class <- ifelse(xgb_prob > 0.5, 1, 0)

evaluate_model(xgb_prob, xgb_class, y_test_num, y_test)

# Feature Importance
importance <- xgb.importance(model = xgb_model)
xgb.plot.importance(importance)

# ============================================
# Random Forest
# ============================================

library(randomForest)

rf_model <- randomForest(x = x_train, y = as.factor(y_train))

rf_prob <- predict(rf_model, x_test, type="prob")[,2]
rf_class <- ifelse(rf_prob > 0.5, 1, 0)

evaluate_model(rf_prob, rf_class, y_test_num, y_test)

# Feature Importance
varImpPlot(rf_model)

