library(dplyr)
library(png)
library(grid)
library(ggplot2)

shots <- read.csv("data/shots.csv")

set.seed(12345)
shots_sample <- shots[sample(nrow(shots), size = 0.10 * nrow(shots)), ]

# Full Rink Heatmap of Made shots

# Filter only successful shots (goal == 1)
shots_made <- shots_sample %>% filter(goal == 1)

rink_img <- readPNG("Images/rink.png")
g <- rasterGrob(rink_img, width = unit(1,"npc"), height = unit(1,"npc"))

# Plot shot locations on top of rink
plot <- ggplot(shots_made, aes(x = xCord, y = yCord)) +
  annotation_custom(g, xmin = min(shots_made$xCord), xmax = max(shots_made$xCord),
                    ymin = min(shots_made$yCord), ymax = max(shots_made$yCord)) +
  geom_point(color = "red", alpha = 0.6, size = 0.2) +
  coord_fixed(ratio = 1) +
  labs(title = "Locations of Made Shots on Hockey Rink",
       x = "X Coordinate",
       y = "Y Coordinate") +
  theme_void()


# shots$shotID <- NULL
# shots <- shots[shots$lastEventCategory != "CHL", ]
# shots <- shots[shots$lastEventTeam != "VIDE", ]
# shots <- shots[shots$lastEventTeam != "GOAL", ]
# shots <- shots[shots$playerPositionThatDidEvent != "G", ]
# shots$isHomeTeam <- NULL
