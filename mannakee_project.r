library(xtable)
library(MASS)
library(lars)
library(plyr)
library(kernlab)
library(e1071)
library(stats)
library(glmnet)
library(cluster)

df <- read.csv('acet_data.csv',header=T,stringsAsFactors=F)
df <- subset(df,select=-X)
df[which(df$enzyme==''),]$enzyme <- 'None'

# Restrict everything to NatB and NatC
#df <- subset(df,enzyme==c('NatB','NatC'))

df$enzyme <- factor(df$enzyme)
# label testing and training data
df$train <- TRUE

# complicated way of splitting the data so that 20% of each enzyme type is reserved for testing
df[sample(which(df$enzyme=='None'),floor(.2*length(which(df$enzyme=='None')))),'train'] <- FALSE
df[sample(which(df$enzyme=='NatA'),floor(.2*length(which(df$enzyme=='NatA')))),'train'] <- FALSE
df[sample(which(df$enzyme=='NatB'),floor(.2*length(which(df$enzyme=='NatB')))),'train'] <- FALSE
df[sample(which(df$enzyme=='NatC'),floor(.2*length(which(df$enzyme=='NatC')))),'train'] <- FALSE

df$test <- !df$train

# Create a numeric class variable
df[which(df$enzyme=='None'),'class'] <- 4 
df[which(df$enzyme=='NatA'),'class'] <- 3 
df[which(df$enzyme=='NatB'),'class'] <- 1 
df[which(df$enzyme=='NatC'),'class'] <- 2 
#df$class <- factor(df$class)




# Use Lars with Cross-Validation to select the best subset of variables
Y.train <- as.matrix(subset(df,train,select=class))

#X.train <- as.matrix(data.matrix((subset(df,train,select=-c(enzyme,train,test,locus,class)))[-1]))
#X.train<- as.matrix(data.matrix(subset(df,train,select=-c(enzyme,train,test,locus,class))))
X.train<- as.matrix(data.matrix(subset(df,train,select=c(P,QN,order_prob,min_hp,max_hp,E,H,aromaticity,max_flex,min_flex))))
lars.obj <- cv.glmnet(X.train,Y.train,alpha=0,nfolds=5,family='multinomial')

Y.test <- as.matrix(subset(df,test,select=class))
X.test<- as.matrix(data.matrix(subset(df,test,select=c(P,QN,order_prob,min_hp,max_hp,E,H,aromaticity,max_flex,min_flex))))
#X.test <- as.matrix(data.matrix(subset(df,test,select=-c(enzyme,train,test,locus,class))))
#X.test <- as.matrix(data.matrix(subset(df,test,select=c(P,Q,N,min_hp,T,E,H))))

lars.pred <- predict(lars.obj,X.test,s='lambda.min',type='class')
#lars.pred
lars.coef <- coef(lars.obj,s='lambda.min')
count(as.character(lars.pred))
count(subset(df,test)$class)
lars.mean <- mean(as.numeric(lars.pred)!=subset(df,test)$class)
# lars fails. classifies everything as NatA.
# create a formula
form <- formula(enzyme ~ E + P + QN  + aromaticity + min_hp + max_hp + C + H + order_prob)
form2 <- formula(enzyme ~ QN + order_prob + E)
# try an lda classifier

lda.obj <- lda(form2,df,subset=train)
#lda.obj <- lda(enzyme ~ P + QN,df,subset=train)
lda.pred <- predict(lda.obj,subset(df,test))
summary(lda.pred$class)
lda.mean <- mean(lda.pred$class!=subset(df,test)$enzyme)
# doesn't work. Classifies everything as NatA


# Try QDA
#qda.obj <- qda(enzyme ~ P + QN + order_prob + min_hp + aromaticity + E + min_flex + max_flex + W,df,subset=train)
qda.obj <- qda(form2,df,subset=train)
#qda.obj <- qda(enzyme ~ P + QN,df,subset=train)
qda.pred <- predict(qda.obj,subset(df,test))
summary(qda.pred$class)
qda.mean <- mean(qda.pred$class!=subset(df,test)$enzyme)

# Try SVM
ksvm.obj <- ksvm(form2,data=subset(df,train),cross=3,C=500)
error(ksvm.obj)
testdf <- subset(df,test)
ksvm.pred <- predict(ksvm.obj,testdf,type='response')
ksvm.mean <- mean(ksvm.pred!=testdf$enzyme)
summary(ksvm.pred)


# Principal Component Analysis
form1 <- formula(~ E + P + QN  + aromaticity + min_hp + max_hp + C + H + order_prob)
prcomp <- princomp(form,subset(df,train))

# K-means clustering
km.fit <- kmeans(subset(df,train,select=c(P,QN,order_prob,E,max_hp,min_hp,aromaticity)),4)

# Heirarchical clustering
d <- dist(subset(df,train,select=c(P,QN,order_prob,E,max_hp,min_hp,aromaticity)),method='euclidean')
hc.fit <- hclust(d,method='ward')
