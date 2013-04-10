library(MASS)

# Load the prostate data, center and scale the predictors, 
# center the response, and subset test and train
pdata <- read.csv('prostate.txt',row.names=1,sep='\t')
pdata <- cbind(scale(pdata[1:8],T,T),pdata[9],pdata[10])
ptrain <- subset(pdata,train)[-10]
ptest <- subset(pdata,!train)[-10]
print(dim(ptest))
# Run linear regression and capture MSE on the test data
# as well as the standard error of the model
prost.lm <- lm(lpsa ~ .,ptrain)
lm.sigma <- summary(prost.lm)$sigma
lm.mse <- mean(squared.err(ptest$lpsa,predict(prost.lm,ptest[1:8])))
print(lm.mse)

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

# little function to calculate squared error
squared.err <- function(y,yhat){
  return((y-yhat)^2)
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
testy <- ptest$lpsa
testyhat <- scale(ptest[1:8],center=T,scale=prost.ridge.test$scales) %*% prost.ridge.test$coef + prost.ridge.test$ym
ridge.se <- apply(testy-testyhat,2,sd)
ridge.mse <- mean(squared.err(testy,testyhat))
print(ridge.mse)
print(ridge.se)


############################################
# Best Subset Regression - Exhaustive
############################################

library(leaps)
prost.exh <- regsubsets(ptrain[1:8],ptrain$lpsa,method='exhaustive')
exh.coefs <- coef(prost.exh,id=1:8)
train.exh.models <- function(coefs){
  # Runs cross validation for each set of coefficients
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

# Calculate MSE for cross-validation on training data of all subsets
exh.mse <- sapply(exh.coefs,train.exh.models)
exh.preds <- names(exh.coefs[[which.min(exh.mse)]])[-1]
print(exh.preds)

exh.test.frame <- cbind(ptrain[9],ptrain[,exh.preds,drop=F])
exh.best.model <- lm(lpsa ~ .,data=exh.test.frame)
exh.yhat <- predict(exh.best.model,ptest[,exh.preds,drop=F])
exh.mse2 <- mean(squared.err(exh.yhat,ptest[9]))
print(exh.mse2)









