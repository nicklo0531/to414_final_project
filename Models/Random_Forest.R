library(randomForest)
library(caret)

# ---- Build train/test data frames ----
train_df <- data.frame(X_train, y_train = as.factor(y_train))
test_df  <- data.frame(X_test,  y_test  = as.factor(y_test))

# ---- Random Forest model ----
rf <- randomForest(
  y_train ~ .,
  data  = train_df,
  ntree = 500
)

# ---- Predict classes on test set ----
rf_pred <- predict(rf, test_df)  # default: predicted class

# ---- Confusion matrix ----
cm_rf <- confusionMatrix(
  rf_pred,
  test_df$y_test,
  positive = "1"
)

cm_rf
