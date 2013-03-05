library(plyr)
library(class)
library(MASS)
library(xtable)

# Load zip code digit data
train = read.table('zip_train.csv')
colnames(train)[1] <- 'digits'
test = read.table('zip_test.csv')
colnames(test)[1] <- 'digits'
# Restrict data to digits 2 and 3 and drop variable 16 which takes a constant value
# for problem 2.8
train1 <- subset(train,as.numeric(digits) %in% c(2,3),select =-17)
train1$digits <- factor(train1$digits)
test1<- subset(test,as.numeric(digits) %in% c(2,3),select=-17)
test1$digits <- factor(test1$digits)
# Restrict data to digits 1,2,and 3 and drop variable 16 for problem 4
train2 <- subset(train,as.numeric(digits) %in% c(1,2,3),select =-17)
train2$digits <- factor(train2$digits)
test2<- subset(test,as.numeric(digits) %in% c(1,2,3),select=-17)
test2$digits <- factor(test2$digits)


# Calculate K-nearest neighbors training and testing error for k={1,3,5,7,15} for problem 2.8
k = c(1,3,5,7,15)
knn.train1.errors <- llply(k,function(x) mean(ifelse(train1$digit != knn(train1[-1],train1[-1],train1$digit,k=x),1,0))*100)
knn.test1.errors <- llply(k,function(x) mean(ifelse(test1$digit != knn(train1[-1],test1[-1],train1$digit,k=x),1,0))*100)
knn.table.1 <- data.frame(rbind(knn.train1.errors,knn.test1.errors),row.names=c('Training Errors','Testing Errors'))
colnames(knn.table.1) <- c('1','3','5','7','15')
table1 <- xtable(knn.table.1,booktabs=T)
# Calculate K-nearest neighbors training and testing error for k={1,3,5,7,15} for problem 4
knn.train2.errors <- llply(k,function(x) mean(ifelse(train2$digit != knn(train2[-1],train2[-1],train2$digit,k=x),1,0))*100)
knn.test2.errors <- llply(k,function(x) mean(ifelse(test2$digit != knn(train2[-1],test2[-1],train2$digit,k=x),1,0))*100)
knn.table.2 <- data.frame(rbind(knn.train2.errors,knn.test2.errors),row.names=c('Training Errors','Testing Errors'))
colnames(knn.table.2) <- c('1','3','5','7','15')
table2 <- xtable(knn.table.2,booktabs=T)

# Create an LDA classifier and record training and test data
# First make the formula without V17 which was removed because it is constant
lda.1 <- lda(digits ~ .,train2)
lda.train2.error <- mean(ifelse(predict(lda.1,train2[-1])$class != train2$digits,1,0)*100)
lda.test2.error <- mean(ifelse(predict(lda.1,test2[-1])$class != test2$digits,1,0)*100)


# Generate Bivariate Normal data for QDA
mu1 <- c(2,1)
sigma1 <- matrix(c(1,-.5,-.5,1),nrow=2)
mu2 <- c(1,2)
sigma2 <- matrix(c(2,1,1,2),nrow=2)
set.seed(52771)
qda.train <- data.frame(rbind(mvrnorm(100,mu1,sigma1),mvrnorm(100,mu2,sigma2)),class=rep(1:2,each=100),train=T,test=F)
qda.test <- data.frame(rbind(mvrnorm(1000,mu1,sigma1),mvrnorm(1000,mu2,sigma2)),class=rep(1:2,each=1000),test=T,train=F)
qda.set <- rbind(qda.train,qda.test)

# Create the function for the Bayes Rule
bayes.rule <- function(row){
  mu1 <- c(2,1)
  sigma1 <- matrix(c(1,-.5,-.5,1),nrow=2)
  mu2 <- c(1,2)
  sigma2 <- matrix(c(2,1,1,2),nrow=2)
  point <- c(row[1],row[2])
  bayes.class <- ifelse((t(point-mu1) %*% solve(sigma1) %*% (point-mu1)) - (t(point-mu2) %*% solve(sigma2) %*% (point-mu2)) < 1.386 ,1,2)
  return(ifelse(bayes.class!=row[3],1,0)) 
}

# Calculate Bayes training and testing error
bayes.train.error <- mean(apply(subset(qda.set,train),1,bayes.rule))*100
bayes.test.error <- mean(apply(subset(qda.set,test),1,bayes.rule))*100

# Calculate LDA training and testing error
lda.2 <- lda(class ~ X1 + X2,subset(qda.set,train))
lda.train3.error <- mean(ifelse(predict(lda.2,subset(qda.set,train,select=c(1,2)))$class != subset(qda.set,train)$class,1,0))*100
lda.test3.error <- mean(ifelse(predict(lda.2,subset(qda.set,test,select=c(1,2)))$class != subset(qda.set,test)$class,1,0))*100

# Calculate QDA training and testing error
qda.1 <- qda(class ~ X1 + X2,subset(qda.set,train))
qda.train.error <- mean(ifelse(predict(qda.1,subset(qda.set,train,select=c(1,2)))$class != subset(qda.set,train)$class,1,0))*100
qda.test.error <- mean(ifelse(predict(qda.1,subset(qda.set,test,select=c(1,2)))$class != subset(qda.set,test)$class,1,0))*100

prob5.train.error <- cbind(bayes.train.error,lda.train3.error,qda.train.error)
prob5.test.error <- cbind(bayes.test.error,lda.test3.error,qda.test.error)

# Create data frame for table printin
prob5.frame <- data.frame(rbind(prob5.train.error,prob5.test.error),row.names=c('Training Error','Test Error'))
colnames(prob5.frame) <- c('Bayes','LDA','QDA')

table3 <- xtable(prob5.frame,booktabs=T,digits=1)




