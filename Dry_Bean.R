library(tidyverse)
library(data.table)
library(lubridate)
library(stats)
library(RColorBrewer)
library(readxl)
library(matrixStats)
library(caret)
library(KODAMA)
library(forcats)

my_data <- read_excel("C:/Users/H13456/Documents/R projects/Dry Bean Dataset/Dry_Bean_Dataset.xlsx")

x_centered <- sapply(sweep(my_data[,-17], 2, colMeans(my_data[,-17])), FUN = function(y){as.numeric(y)})
x_scaled <- sweep(x_centered, 2, colSds(x_centered), FUN = "/")

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
  axis(side = 2, 1:ncol(x), rev(colnames(x)), las = 2)
}

my_image(cor(x_scaled), zlim = c(-1,1))

d_samples <- dist(x_scaled)
dist_SEKERtoSEKER <- as.matrix(d_samples)[1, my_data$Class == "SEKER"]
mean(dist_SEKERtoSEKER[2:length(dist_SEKERtoSEKER)])
dist_SEKERtoCALI <- as.matrix(d_samples)[1, my_data$Class == "CALI"]
mean(dist_SEKERtoSEKER[1:length(dist_SEKERtoCALI)])
dist_SEKERtoDERMASON <- as.matrix(d_samples)[1, my_data$Class == "DERMASON"]
mean(dist_SEKERtoDERMASON[1:length(dist_SEKERtoDERMASON)])


d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

h <- hclust(d_features)
groups <- cutree(h, k = 3)
split(names(groups), groups)

pca <- prcomp(x_scaled)
summary(pca)

data.frame(pca$x[,1:2], type = my_data$Class) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()


data.frame(type = my_data$Class, pca$x[,1:5]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(my_data$Class, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- my_data$Class[test_index]
train_x <- x_scaled[-test_index,]
train_y <- my_data$Class[-test_index]


train_rf <- train(train_x, as.factor(train_y), method = "rf", importance = TRUE)
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)
varImp(train_rf)

train_Rborist <- train(train_x, as.factor(train_y), method = "Rborist", importance = TRUE)
Rborist_preds <- predict(train_Rborist, test_x)
mean(Rborist_preds == test_y)

train_lda <- train(train_x, as.factor(train_y), method = "lda")
lda_preds <- predict(train_lda, test_x)
mean(lda_preds == test_y)

train_qda <- train(train_x, as.factor(train_y), method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(qda_preds == test_y)

train_knn <- train(train_x, as.factor(train_y), method = "knn")
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)

train_AdaBag <- train(train_x, as.factor(train_y), method = "AdaBag")
AdaBag_preds <- predict(train_AdaBag, test_x)
mean(AdaBag_preds == test_y)

ensemble <- data.frame(rf = rf_preds,
                  Rborist = Rborist_preds,
                  knn = knn_preds)

j <- seq(1, nrow(ensemble))
ensemble_preds <- sapply(j, function(l){
  ensemble[l, which.max(match(ensemble[l,], unique(ensemble[l,])))]
  })
mean(ensemble_preds == test_y)
