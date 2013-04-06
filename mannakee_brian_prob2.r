require(ggplot2)
require(class)
require(MASS)
require(plyr)
source('prob1.r')

# Create a function definition identical to the Bayes boundary in Problem 2 and then find its roots
bayes.bound <- function(x){
  ((1/sqrt(2*pi)*exp(-(x^2/2)))/((0.65/sqrt(2*pi))*exp(-((x-1)^2/2)) + (0.35/sqrt(8*pi))*exp(-((x+1)^2/8))))
}

plot.bayes.range <- function(){
  x <- seq(-3,3,by=.01)
  y <- lapply(x,bayes.bound)
  
  plot(x,y,xlab='X',ylab='Likelihood Ratio')
  abline(h=1.5)
  abline(v=.274)
  abline(v=-1.58)
  axis(1,at=c(-1.58,.274))
}

# create a function to return test and training error at different k values

k.means.errors <- function(){
  # returns a data frame with testing and training error
  # for each value in k. Assumes the testing and
  # training data frames from prob1.r are available
  k = c(1,4,7,10,13,16,30,45,60,80,100,150,200)
  s1.train.error <- lapply(k,function(x) mean(scenario.1.train$Class!=knn(scenario.1.train[1:2],scenario.1.train[1:2],scenario.1.train$Class,k=x))*100)
  s1.test.error <- lapply(k,function(x) mean(scenario.1.test$Class!=knn(scenario.1.train[1:2],scenario.1.test[1:2],scenario.1.train$Class,k=x))*100)
  s2.train.error <- lapply(k,function(x) mean(scenario.2.train$Class!=knn(scenario.2.train[1:2],scenario.2.train[1:2],scenario.2.train$Class,k=x))*100)
  s2.test.error <- lapply(k,function(x) mean(scenario.2.test$Class!=knn(scenario.2.train[1:2],scenario.2.test[1:2],scenario.2.train$Class,k=x))*100)
  df <- data.frame(rbind(s1.train.error,s1.test.error,s2.train.error,s2.test.error))
  colnames(df) <- c('1','4','7','10','13','16','30','45','60','80','100','150','200')
  rownames(df) <- c('Scen 1 Training Error','Scen 1 Testing Error','Scen 2 Training Error','Scen 2 Testing Error')
  return(df)
}


  
plot.k.means.1 <- function(){
  df <- k.means.errors()
  x <- as.integer(colnames(df))
  TestN <- 1000
  TrainingN <- 200
  plot(TrainingN/x,t(df[1,]),col='red',type='b',xlab='Degrees of Freedom',ylab='Percent Error',ylim=c(0,70),xlim=c(0,260),main='Scenario 1')
  points(TestN/x,t(df[2,]),col='blue',type='b')
  legend('topright',c('Train','Test'),fill=c('red','blue'))
}

plot.k.means.2 <- function(){
  df <- k.means.errors()
  x <- as.integer(colnames(df))
  TestN <- 1000
  TrainingN <- 200
  plot(TrainingN/x,t(df[3,]),col='red',type='b',xlab='Degrees of Freedom',ylab='Percent Error',ylim=c(0,60),xlim=c(0,260),main='Scenario 2')
  points(TestN/x,t(df[4,]),col='blue',type='b')
  legend('topright',c('Train','Test'),fill=c('red','blue'))
}

# Perform logistic regression classification on Scenario one and two
# sets, add the logistic regression classification and errors to the data frames
  
logist.1 <- glm(Class ~ X1 + X2,family=binomial(link='logit'),scenario.1.train)
logist.2 <- glm(Class ~ X1 + X2,family=binomial(link='logit'),scenario.2.train)
scenario.1.train$Log.mod <- unlist(lapply(predict(logist.1,scenario.1.train[1:2]), function(x) ifelse(x>0,1,0)))
scenario.2.train$Log.mod <- unlist(lapply(predict(logist.2,scenario.2.train[1:2]), function(x) ifelse(x>0,1,0)))
scenario.1.test$Log.mod <- unlist(lapply(predict(logist.1,scenario.1.test[1:2]), function(x) ifelse(x>0,1,0)))
scenario.2.test$Log.mod <- unlist(lapply(predict(logist.2,scenario.2.test[1:2]), function(x) ifelse(x>0,1,0)))
scenario.1.test$Log.error <- apply(scenario.1.test,1,function(row) ifelse(row[3] != row[8],1,0))
scenario.1.train$Log.error <- apply(scenario.1.train,1,function(row) ifelse(row[3] != row[8],1,0))
scenario.2.test$Log.error <- apply(scenario.2.test,1,function(row) ifelse(row[3] != row[6],1,0))
scenario.2.train$Log.error <- apply(scenario.2.train,1,function(row) ifelse(row[3] != row[6],1,0))

# Do the same thing for an LDA classifier
lda.1 <- lda(Class ~ X1 + X2,scenario.1.train)
lda.2 <- lda(Class ~ X1 + X2,scenario.2.train)
scenario.1.train$Lda.mod <- predict(lda.1,scenario.1.train)$class
scenario.1.test$Lda.mod <- predict(lda.1,scenario.1.test)$class
scenario.2.train$Lda.mod <- predict(lda.2,scenario.2.train)$class
scenario.2.test$Lda.mod <- predict(lda.2,scenario.2.test)$class
scenario.1.train$Lda.error <- apply(scenario.1.train,1,function(row) ifelse(row[3] != row[10],1,0))
scenario.1.test$Lda.error <- apply(scenario.1.test,1,function(row) ifelse(row[3] != row[10],1,0))
scenario.2.train$Lda.error <- apply(scenario.2.train,1,function(row) ifelse(row[3] != row[8],1,0))
scenario.2.test$Lda.error <- apply(scenario.2.test,1,function(row) ifelse(row[3] != row[8],1,0))














