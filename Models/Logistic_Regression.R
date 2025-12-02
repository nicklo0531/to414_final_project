library(caret)

glm_model <- glm(goal ~ ., data = shots_train, family = binomial)
  
# Run stepwise backward selection
step_model <- step(full_model, direction = "backward", trace = 0)

summary(m1)

pred_step <- predict(m1, shots_test, type="response")
pred_binary_step <- ifelse(shots_pred_m1 >= .1, 1, 0)
summary(pred_binary_step)

cm_step <- confusionMatrix(as.factor(pred_binary_step),as.factor(shots_test$goal),positive = "1")
cm_step
summary(step_model)

# Save trained Stepwise Logistic Regression model for Shiny
saveRDS(step_model, file = "Models/step_model.rds")

