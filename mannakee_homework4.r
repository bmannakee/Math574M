library(MASS)
library(lars)
library(leaps)
library(xtable)
# Load the prostate data, center and scale the predictors, 
# center the response, and subset test and train
pdata <- read.csv('prostate.txt',row.names=1,sep='\t')
pdata <- cbind(scale(pdata[1:8],T,T),pdata[9],pdata[10])
ptrain <- subset(pdata,train)[-10]
ptest <- subset(pdata,!train)[-10]
testy <- ptest[9]



# little function to calculate squared error
squared.err <- function(y,yhat){
  return((y-yhat)^2)
}

# Run linear regression and capture MSE on the test data
# as well as the standard error of the model

prost.lm <- lm(lpsa ~ .,ptrain)
lm.sigma <- summary(prost.lm)$sigma
yhat.lm <- predict(prost.lm,ptest[1:8])
lm.mse <- mean(squared.err(testy,yhat.lm))
lm.se <- apply(testy-yhat.lm,2,sd)
print('lm')
print(lm.mse)
print(lm.se)

# Need a function to fold the data

get_folded_indexes <- function(n,k){
  # returns a list with k lists each 
  # with random indices
  # adapted from github code by mcmtroffaes
  set.seed(52772)
  ixs <- 1:n
  sizes <- c()
  ix_list <- list()
  for (i in 1:k){
    first <- 1 + (((i-1)*n) %/% k)
    last <- ((i * n) %/% k)
    sizes <- append(sizes, last - first + 1)
  }
  for (j in 1:k){
    s <- sample(ixs,sizes[j])
    ixs <- setdiff(ixs,s)
    ix_list[[j]] <- s
  }
  return(ix_list)
}




cv_folds <- get_folded_indexes(67,5)
#########################
# Ridge Regression
#########################

#compare the GCV results to my cross-validation
prost.ridge <- lm.ridge(lpsa ~ .,data=ptrain, lambda=seq(0,10,.1))
gcv.lambda <- which.min(prost.ridge$GCV)
gcv.ridge <- lm.ridge(lpsa ~ .,data=ptrain,lambda=gcv.lambda)
gcv.testyhat <- as.matrix(ptest[1:8]) %*% gcv.ridge$coef + gcv.ridge$ym
print(mean(squared.err(ptest[9],gcv.testyhat)))

mse <- c()
lambda=seq(0,10,.1)
for (i in lambda){
  err <- c()
  for (j in 1:length(cv_folds)){
    train_folds <- unlist(cv_folds[-j])
    test_fold <- unlist(cv_folds[j])
    cv_ridge <- lm.ridge(lpsa ~ .,data = ptrain, subset=train_folds,lambda=i)
    y <- ptrain[test_fold,9]
    yhat <- scale(ptrain[test_fold,1:8],center=F,scale=cv_ridge$scales) %*% cv_ridge$coef + cv_ridge$ym
    err <- append(err,squared.err(y,yhat))
  }
  mse <- append(mse,mean(err))
}
best_lambda <- lambda[which.min(mse)]
prost.ridge.test <- lm.ridge(lpsa ~ .,data=ptrain,lambda=best_lambda)

testyhat <- scale(ptest[1:8],center=T,scale=prost.ridge.test$scales) %*% prost.ridge.test$coef + prost.ridge.test$ym
ridge.se <- apply(testy-testyhat,2,sd)
ridge.mse <- mean(squared.err(testy,testyhat))
print('ridge')
print(ridge.mse)
print(ridge.se)


############################################
# Best Subset Regression - Exhaustive
############################################

train.subset.models <- function(coefs){
  # Runs cross validation for a model
  # with coefficient names in coefs
  err <- c()
  for (i in 1:length(cv_folds)){
    train_folds <- unlist(cv_folds[-i])
    test_fold <- unlist(cv_folds[i])
    # make a data frame for lm, need drop=F to make the one variable case
    # return a frame instead of a list, which is a huge pain
    train.frame <- cbind(ptrain[train_folds,9,drop=F],ptrain[train_folds,names(coefs[-1]),drop=F])
    model <- lm(lpsa ~ .,train.frame)
    yhat <- predict(model,ptrain[test_fold,names(coefs[-1]),drop=F])
    y <- ptrain[test_fold,9]
    err <- append(err,squared.err(yhat,y))
  }
  mse <- mean(err)
  return(mse)
}

library(leaps)
prost.exh <- regsubsets(ptrain[1:8],ptrain$lpsa,method='exhaustive')
exh.coefs <- coef(prost.exh,id=1:8)


# Calculate MSE for cross-validation on training data of all subsets
exh.mse <- sapply(exh.coefs,train.subset.models)
exh.preds <- names(exh.coefs[[which.min(exh.mse)]])[-1]
exh.test.frame <- cbind(ptrain[9],ptrain[,exh.preds,drop=F])
exh.best.model <- lm(lpsa ~ .,data=exh.test.frame)
exh.yhat <- predict(exh.best.model,ptest[,exh.preds,drop=F])
exh.mse2 <- mean(squared.err(exh.yhat,ptest[9]))
exh.se <- apply(testy-exh.yhat,2,sd)
print('bss_exh')
print(exh.preds)
print(exh.mse2)

##########################################
# Best Subset Regression - Forward
##########################################

prost.fwd <- regsubsets(ptrain[1:8],ptrain$lpsa,method='forward')
fwd.coefs <- coef(prost.fwd,id=1:8)


# Calculate MSE for cross-validation on training data of all subsets
fwd.mse <- sapply(fwd.coefs,train.subset.models)
fwd.preds <- names(fwd.coefs[[which.min(fwd.mse)]])[-1]
fwd.test.frame <- cbind(ptrain[9],ptrain[,fwd.preds,drop=F])
fwd.best.model <- lm(lpsa ~ .,data=fwd.test.frame)
fwd.yhat <- predict(fwd.best.model,ptest[,fwd.preds,drop=F])
fwd.mse2 <- mean(squared.err(fwd.yhat,ptest[9]))
fwd.se <- apply(fwd.yhat-testy,2,sd)
print('bss_fwd')
print(fwd.preds)
print(fwd.mse2)
print(fwd.se)


########################################
# LASSO variable selection
########################################


# Get the L1 norm with the lowest MSE
# Suppress the plot.
prost.cv.lasso <- cv.lars(as.matrix(ptrain[1:8]),as.matrix(ptrain[9]),K=5,type='lasso',plot.it=F)
prost.s <- prost.cv.lasso$index[which.min(prost.cv.lasso$cv)]

# Run lasso
prost.lasso <- lars(as.matrix(ptrain[1:8]),as.matrix(ptrain[9]),type='lasso')
yhat.lasso <-  predict(prost.lasso,as.matrix(ptest[1:8]),s=prost.s,type='fit',mode='fraction')$fit
lasso.mse <- mean(squared.err(yhat.lasso,testy))
lasso.se <- apply(testy-yhat.lasso,2,sd)
print('lasso')
print(lasso.mse)
print(lasso.se)

###################
# Make a nice table
###################

table.mse <- c(lm.mse,ridge.mse,exh.mse2,fwd.mse2,lasso.mse)
table.se <- c(lm.se,ridge.se,exh.se,fwd.se,lasso.se)
table.frame <- data.frame(cbind(table.mse,table.se))
names(table.frame) <- c('Mean Squared Error','Standard Error')
row.names(table.frame) <- c('Linear model','Ridge regression','Exhaustive best subset selection','Forward best subset regression','Lasso')
prob5.table <- xtable(table.frame,booktabs=T,caption='Results of variable selection for prostate data')
