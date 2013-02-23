library(plyr)
library(class)


train = read.table('zip_train.csv')
colnames(train)[1] <- 'digits'
train$digits <- as.factor(train$digits)
train <- subset(train,as.numeric(digits) < 4)
test = read.table('zip_test.csv')
colnames(test)[1] <- 'digits'
test$digits <- as.factor(test$digits)
test<- subset(test,as.numeric(digits) < 4)


# Calculate K-nearest neighbors training and testing error for k={1,3,5,7,15}
k = c(1,3,5,7,15)
knn.train.errors <- llply(k,function(x) mean(ifelse(train$digit != knn(train[-1],train[-1],train$digit,k=x),1,0))*100)
knn.test.errors <- llply(k,function(x) mean(ifelse(test$digit != knn(train[-1],test[-1],train$digit,k=x),1,0))*100)