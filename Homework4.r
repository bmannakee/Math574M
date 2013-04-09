

# Load the prostate data, center and scale the predictors, 
# center the response, and subset test and train
pdata <- read.table('prostate.txt')
pdata <- cbind(scale(pdata[1:8]),scale(pdata[9],TRUE,FALSE),pdata[10])
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
  print(sizes)
  for (j in 1:k){
    s <- sample(ixs,sizes[j])
    ixs <- setdiff(ixs,s)
    ix_list[[j]] <- s
  }
  return(ix_list)
}

