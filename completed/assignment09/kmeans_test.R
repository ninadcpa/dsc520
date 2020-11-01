library(ggplot2)
library(ggfortify)
library(caTools)
library(knitr)
library(pander)
library(class)

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
str(confusion_matrix)
confusion_matrix
confusion_matrix$Accuracy
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)
accuracy <-100 * sum(test_bi_data$label == predicted.values)/NROW(test_bi_data$label)
accuracy

# we want to test with 6 different values for k, hence creating vector of length 6
accuracy_vals <- c()

for (i in k_vals) {
  predicted.values <- knn(training_bi_data[2:3], test_bi_data[2:3], training_bi_data$label, k = i)
  accuracy_vals <-c(accuracy_vals,100 * sum(test_bi_data$label == predicted.values)/NROW(test_bi_data$label))
}
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

autoplot(kmeans(bi_df, 3), data = bi_df,
         label = TRUE, label.size = 3, frame = TRUE)