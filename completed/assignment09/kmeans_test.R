library(ggplot2)
library(ggfortify)
library(caTools)
library(knitr)
library(pander)
library(class)
library(tidyverse)
library(factoextra)

setwd("/cloud/project/completed/assignment09")
bi_df <- read.csv("binary-classifier-data.csv")
str(bi_df)
tri_df <- read.csv("trinary-classifier-data.csv")
str(tri_df)

ggplot(bi_df, aes(x=x, y=y)) +
  geom_point(shape=18, color="blue")

ggplot(tri_df, aes(x=x, y=y)) +
  geom_point(shape=18, color="orange")

set.seed(123)

k_vals <- c(3,5,10,15,20,25)

sample <- sample.split(bi_df$label, SplitRatio = 0.70)
training_bi_data = subset(bi_df, sample == TRUE)
test_bi_data = subset(bi_df, sample == FALSE)

predicted.values <- knn(training_bi_data[2:3], test_bi_data[2:3], training_bi_data$label, k = 5)
library(caret)
confusion_matrix <- confusionMatrix(table(predicted.values  ,test_bi_data$label))
accuracy <- ((confusion_matrix[1,1] + confusion_matrix[2, 2]) / sum(confusion_matrix))
confusion_matrix
confusion_matrix$Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)
accuracy <-100 * sum(test_bi_data$label == predicted.values)/NROW(test_bi_data$label)
accuracy

# we want to test with 6 different values for k, hence creating vector of length 6
accuracy_vals <- c()

iterations = 6
variables = 2

output <- matrix(ncol=variables, nrow=iterations)

for (i in k_vals) {
  sprintf(i)
}

for (i in k_vals) {
  predicted.values <- knn(training_bi_data[2:3], test_bi_data[2:3], training_bi_data$label, k = i)
  #output[i,] <- 100 * sum(test_bi_data$label == predicted.values)/NROW(test_bi_data$label)
  accuracy_vals <-c(accuracy_vals,100 * sum(test_bi_data$label == predicted.values)/NROW(test_bi_data$label))
}
output
accuracy_vals

k_accuracy_df <- data.frame(k_vals, accuracy_vals)
str(k_accuracy_df)

ggplot(data=k_accuracy_df, aes(x=k_vals, y=accuracy_vals, group=1)) +
  geom_line(linetype = "dashed")+
  labs(
    title = "K vs Accuracy",
    x = "Value of K",
    y = "Accuracy %"
  ) +
  geom_point()

autoplot(kmeans(bi_df, 10), data = bi_df,
         label = TRUE, label.size = 3, frame = TRUE)

######################## Ex 16 #################
setwd("/cloud/project/")

ex16_data <- read.csv("data/clustering-data.csv")
head(ex16_data)
ggplot(ex16_data, aes(x=x, y=y)) +
  geom_point(shape=18, color="orange")

kmean_output <- kmeans(ex16_data, 3)
summary(kmean_output)
for (i in 2:12) {
  print(i)
}
autoplot(kmeans(ex16_data, 3), data = ex16_data, 
         label = TRUE, label.size = 3, frame = TRUE)

set.seed(123)
for(i in 2:12){
  set.seed(123)
  ex16_data <- read.csv("data/clustering-data.csv")
  ex16_data.cluster <- kmeans(ex16_data, i)
  ex16_data$cluster <- as.factor(ex16_data.cluster$cluster)
  p <- ggplot(data = ex16_data, 
              aes(x = x, 
                  y = y, 
                  color = cluster)) + 
    geom_point(size = 0.3) + 
    geom_point(data = as.data.frame(ex16_data.cluster$centers), 
               color = "black", 
               shape = 20, 
               size = 3) + 
    ggtitle(paste("K-Means with k == ", i, sep ="")) +
    theme_bw()
  print(p)
}
fviz_nbclust(ex16_data, kmeans, method = "wss")
# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(ex16_data, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 2:12

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)
wss_values
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")