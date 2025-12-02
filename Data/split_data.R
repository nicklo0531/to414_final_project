library(dplyr)

shots <- read.csv("data/shots.csv")

# Remove target from predictors and create dummy variables
shots_no_goal <- shots %>% select(-goal)
shots_dum <- as.data.frame(model.matrix(~ . - 1, data = shots_no_goal))

# Min-max scaling
minmax <- function(x) (x - min(x)) / (max(x) - min(x))
shots_scaled <- as.data.frame(lapply(shots_dum, minmax))
shots_scaled$goal <- shots$goal

# Features and target
X <- shots_scaled %>% select(-goal)
y <- shots_scaled$goal

# Train-test split (50-50)
set.seed(12345)
n <- nrow(X)
train_index <- sample(seq_len(n), size = 0.5 * n)
test_index  <- setdiff(seq_len(n), train_index)

X_train <- X[train_index, , drop = FALSE]
X_test  <- X[test_index, , drop = FALSE]

y_train <- factor(y[train_index], levels = c(0, 1))
y_test  <- factor(y[test_index],  levels = c(0, 1))
