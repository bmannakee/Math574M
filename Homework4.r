library(MASS)

# Load the prostate data, center and scale the predictors, 
# center the response, and subset test and train
pdata <- read.table('prostate.txt')
pdata <- cbind(scale(pdata[1:8]),pdata[9],pdata[10])
ptrain <- subset(pdata,train)[-10]
ptest <- subset(pdata,!train)[-10]

# Run linear regression and capture MSE on the test data
# as well as the standard error of the model
prost.lm <- lm(lpsa ~ .,ptrain)
lm.sigma <- summary(prost.lm)$sigma
lm.mse <- mean(ptest$lpsa - predict(prost.lm,ptest[1:8]))

# Need a function to fold the data

get_folded_indexes <- function(n,k){
  # returns a list with k lists each 
  # with random indices
  # adapted from github code by mcmtroffaes
  set.seed(52771)
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


cv_folds <- get_folded_indexes(67,10)
#########################
# Ridge Regression
#########################

prost.ridge <- lm.ridge(lpsa ~ .,data=ptrain, lambda=seq(0,10,.1))

# Something here is exceptionally sensitive
# to the number of folds. This is perhaps not that surprising.
# I can't find a fold number that matches the GCV output.
# Best match is k=3, but I am going with 5


mse <- c()
lambda=seq(0,10,.1)
for (i in 1:length(lambda)){
  err <- c()
  for (j in length(cv_folds)){
    train_folds <- unlist(cv_folds[-j])
    test_fold <- unlist(cv_folds[j])
    cv_ridge <- lm.ridge(lpsa ~ .,data = ptrain, subset=train_folds,lambda=i)
    y <- ptrain[unlist(cv_folds[j]),9]
    yhat <- as.matrix(ptrain[test_fold,1:8]) %*% cv_ridge$coef + cv_ridge$ym
    err <- append(err,squared.err(y,yhat))
  }
  mse <- append(mse,mean(err))
}
best_lambda <- lambda[which.min(mse)]
print(best_lambda)
prost.ridge.test <- lm.ridge(lpsa ~ .,data=ptest,lambda=best_lambda)
testy <- ptest$lpsa
testyhat <- as.matrix(ptest[1:8]) %*% prost.ridge.test$coef + prost.ridge.test$ym
ridge.mse <- mean(squared.err(testy,testyhat))
print(ridge.mse)
