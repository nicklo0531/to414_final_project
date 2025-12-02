##############################################
# FINAL KNN MODEL – FIXED VERSION
##############################################

library(caret)
library(class)

# 1. Build datasets
train_df <- data.frame(goal = factor(y_train, levels = c(0,1)), X_train)
test_df  <- data.frame(goal = factor(y_test, levels = c(0,1)),  X_test)

# 2. Convert categorical variables to numeric
dummies <- dummyVars(goal ~ ., data = train_df)
train_numeric <- data.frame(predict(dummies, newdata = train_df))
test_numeric  <- data.frame(predict(dummies, newdata = test_df))

train_numeric$goal <- train_df$goal
test_numeric$goal  <- test_df$goal

# 3. REMOVE the columns causing NA during scaling
bad_cols <- c("lastEventCategoryCHL", "lastEventTeamVIDE", "lastEventTeamGOAL")

train_numeric <- train_numeric[, !(names(train_numeric) %in% bad_cols)]
test_numeric  <- test_numeric[, !(names(test_numeric) %in% bad_cols)]

# 4. Remove zero-variance predictors
nzv <- nearZeroVar(train_numeric)
train_numeric <- train_numeric[, -nzv]
test_numeric  <- test_numeric[, -nzv]

# 5. Identify predictor columns
predictors <- setdiff(names(train_numeric), "goal")

# 6. Scale predictors
train_scaled <- scale(train_numeric[, predictors])
test_scaled  <- scale(test_numeric[, predictors])

# Check for NAs
cat("NAs in train_scaled:", sum(is.na(train_scaled)), "\n")
cat("NAs in test_scaled:",  sum(is.na(test_scaled)),  "\n")

# 7. Run KNN
k_value <- 5

pred_knn <- knn(
  train = train_scaled,
  test  = test_scaled,
  cl    = train_numeric$goal,
  k     = k_value
)

# 8. Confusion matrix
pred_knn <- factor(pred_knn, levels = c("0", "1"))
cm_knn <- confusionMatrix(pred_knn, test_numeric$goal, positive = "1")
cm_knn
