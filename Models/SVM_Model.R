library(e1071)
library(caret)

set.seed(12345)

# Ensure targets are factors
y_train <- factor(y_train, levels = c(0,1))
y_test  <- factor(y_test,  levels = c(0,1))

# Combine target and predictors into one flat data frame
train_df <- cbind(y_train = y_train, X_train)

# Fit a linear SVM
svm_model <- svm(y_train ~ ., data = train_df, kernel = "linear", cost = 1, probability = FALSE)

# Predict on full test set
pred <- factor(predict(svm_model, X_test), levels = c(0,1))

# Confusion matrix
best_cm <- confusionMatrix(pred, y_test, positive = "1")

# Display results
best_cm
