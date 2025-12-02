library(dplyr)
library(png)
library(grid)
library(ggplot2)

shots <- read.csv("data/shots.csv")

set.seed(12345)
shots_sample <- shots[sample(nrow(shots), size = 0.10 * nrow(shots)), ]

# Remove target from predictors and create dummy variables
shots_no_goal <- shots_sample %>% select(-goal)
shots_dum <- as.data.frame(model.matrix(~ . - 1, data = shots_no_goal))

# Min-max scaling
minmax <- function(x) (x - min(x)) / (max(x) - min(x))
shots_scaled <- as.data.frame(lapply(shots_dum, minmax))
shots_scaled$goal <- shots_sample$goal

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

# Load data
shots <- read.csv("data/shots.csv")

# Filter only successful shots (goal == 1)
shots_made <- shots %>% filter(goal == 1)

rink_img <- readPNG("Images/rink.png")
g <- rasterGrob(rink_img, width = unit(1,"npc"), height = unit(1,"npc"))

# Plot shot locations on top of rink
ggplot(shots_made, aes(x = xCord, y = yCord)) +
  annotation_custom(g, xmin = min(shots_made$xCord), xmax = max(shots_made$xCord),
                    ymin = min(shots_made$yCord), ymax = max(shots_made$yCord)) +
  geom_point(color = "red", alpha = 0.6, size = 0.2) +
  coord_fixed(ratio = 1) +
  labs(title = "Locations of Made Shots on Hockey Rink",
       x = "X Coordinate",
       y = "Y Coordinate") +
  theme_void()
