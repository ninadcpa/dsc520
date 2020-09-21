# Assignment: Week-3 Exercise-5 American Community Survey 
# Name: Patkhedkar, Ninad
# Date: 2020-09-19

options(scipen=999)  # turn-off scientific notation like 1e+48
library(ggplot2)

acs_14_df <- read.csv("data/acs-14-1yr-s0201.csv")
str(acs_14_df)
cat("\n number of rows - ",nrow(acs_14_df)) 
cat("\n number of rows - ",ncol(acs_14_df))
cat("\n value range for HSDegree - ",range(acs_14_df$HSDegree))
cat("\n Displaying sample records from dataset\n")
head(acs_14_df)

p <- ggplot(acs_14_df, aes(x=HSDegree)) + 
  geom_histogram(color="darkblue", fill="lightblue",binwidth =0.5) +
  labs(
    title = "Americal Community High School Degree Histogram",
    caption = "Data from acs-14-1yr-s0201.csv",
    x = "High School Degree percentage"
  ) +
  geom_vline(aes(xintercept=median(HSDegree)),
             color="orange", size=1, ) +
  geom_vline(aes(xintercept=mean(HSDegree)),
             color="green", size=1)
p

p <- ggplot(acs_14_df, aes(x=HSDegree)) + 
  geom_histogram(aes(y=..density..),color="darkblue", fill="lightblue",binwidth =0.5) +
  labs(
    title = "Americal Community High School Degree Probability Distribution",
    caption = "Data from acs-14-1yr-s0201.csv",
    x = "High School Degree percentage"
  ) +
  geom_density(alpha=.2, fill="#FF6666") 
p

# Display Stats
library(pastecs)
cat("\n Displaying Statistics of American Community Survey\n")
stat.desc(acs_14_df,basic=TRUE, desc=TRUE, norm=FALSE)


library(psych)
description <- describe(acs_14_df$HSDegree)
cat("\n Displaying Data Skew - ",description$skew)


cat("\n Displaying Kurtosis - ",description$kurtosis)

cat("\n Displaying z-score\n")
scale(acs_14_df$HSDegree)