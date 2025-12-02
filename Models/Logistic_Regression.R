library(caret)

glm_model <- glm(goal ~ ., data = shots_train, family = binomial)
  
# Run stepwise backward selection
m1 <- step(full_model, direction = "backward", trace = 0)
  
# Save both models
saveRDS(full_model, "full_model.rds")
saveRDS(m1, "m1.rds")
}

summary(m1)

shots_pred_m1 <- predict(m1, shots_test, type="response")
shots_pred_binary_m1 <- ifelse(shots_pred_m1 >= .1, 1, 0)
summary(shots_pred_binary_m1)

cm_test <- confusionMatrix(as.factor(shots_pred_binary_m1),as.factor(shots_test$goal),positive = "1")
cm_test
summary(m1)