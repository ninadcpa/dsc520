# Assignment: Week-2 Exercise-3 
# Name: Patkhedkar, Ninad
# Date: 2020-09-12

options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)

testscores <- read.csv("data/scores.csv")
head(testscores)
str(testscores)
summary(testscores)

regular_scores <- subset(testscores, Section == "Regular")
head(regular_scores)
str(regular_scores)
summary(regular_scores)

sports_scores <- subset(testscores, Section == "Sports")
head(sports_scores)
str(sports_scores)
summary(sports_scores)

ggplot(testscores, aes(Score, Count)) +
  geom_point(aes(color = Score)) +
  geom_smooth(se = FALSE, method = "lm") +
  labs(
    title = "Regular vs Sports Students performance",
    subtitle = "Scores vs Count",
    caption = "Data from Scores.csv",
    x = "Student scores",
    y = "Student count"
  ) +
  facet_grid(~Section)

p <- ggplot(testscores, aes(x=Score)) + 
  geom_density() +
  labs(
    title = "Density view",
    subtitle = "Scores of Regular and Sports Students",
    caption = "Data from Scores.csv"
  ) +
  geom_vline(aes(xintercept=mean(Score)),
             color="blue", linetype="dashed", size=1) + # Add mean line
  facet_grid(~Section)
p

# Basic box plot
p <- ggplot(testscores, aes(x=Score, y=Count)) + 
  geom_boxplot() +
  coord_flip() +  # flipping plot
  facet_grid(~Section)
p

median(regular_scores$Score) > median(sports_scores$Score)
