bankData <- read.csv(file='D:/Data Minning/finalbankdata.csv',sep=',',header=TRUE)
set.seed(90)
create_train_test <- function(bankData, size = 0.8, train = TRUE) {
  n_row = nrow(bankData)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (bankData[train_sample, ])
  } else {
    return (bankData[-train_sample, ])
  }
}
data_train <- create_train_test(bankData, 0.8, train = TRUE)
data_test <- create_train_test(bankData, 0.8, train = FALSE)
library(rpart)
info_dt <- rpart(y~., data = data_train, method = 'class', parms=list(split='information'))
library(rpart.plot)
rpart.plot(info_dt,extra=104)
data_train$month <- as.factor(data_train$month)
data_test$month <- as.factor(data_test$month)
data_test$month <-droplevels(data_test$month,exclude = "sep")
predict_unseen <- predict(info_dt,data_test,type = 'class')
table_info <- table(data_test$y,predict_unseen)
recall <- table_info[1,1]/(sum(table_info[,1]))
precision <- table_info[1,1]/(sum(table_info[1,]))
F1 <- 2*precision*recall/(precision+recall)
accurary <- sum(diag(table_info))/sum(table_info)