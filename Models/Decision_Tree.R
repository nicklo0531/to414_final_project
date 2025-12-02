library(C50)
library(caret)

train_dt <- cbind(goal = y_train, X_train)
test_dt  <- cbind(goal = y_test,  X_test)

dt_model <- C5.0(goal ~ ., data = train_dt)

pred_dt <- predict(dt_model, test_dt)

pred_dt <- factor(pred_dt, levels = c(0,1))
test_dt$goal <- factor(test_dt$goal, levels = c(0,1))

cm_dt <- confusionMatrix(pred_dt, test_dt$goal, positive = "1")
cm_dt

# Save trained Decision Tree model for Shiny
saveRDS(dt_model, file = "Models/dt_model.rds")