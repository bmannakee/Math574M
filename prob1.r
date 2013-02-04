require(MASS)
require(ggplot2)
set.seed(52771)

# Generate Scenario One Green Data
s1.green <- mvrnorm(n=100,mu=c(2,1),Sigma=diag(2))
s1.green<- data.frame(s1.green)
colnames(s1.green) <- c('x','y')
s1.green$Class <- 'Green'

#Generate Scenario One Red Data
s1.red <- mvrnorm(n=100,mu=c(1,2),Sigma=diag(2))
s1.red<- data.frame(s1.red)
colnames(s1.red) <- c('x','y')
s1.red$Class <- 'Red'

# Create a data frame for Scenario 1
# including the red and green data
scenario.1 <- rbind(s1.green,s1.red)

# Generate Scenario One Means
green.means <- mvrnorm(n=10,mu=c(0,1),Sigma=diag(2))
red.means <- mvrnorm(n=10,mu=c(0,1),Sigma=diag(2))

# Create matrix for Scenario Two
# Red class
s2.red <- matrix(nrow=100,ncol=2)
for (i in 1:100) {
  mean.1 <- sample(red.means[,1],1)
  mean.2 <- sample(red.means[,2],1)
  s2.red[i,]=mvrnorm(n=1,mu=c(mean.1,mean.2),Sigma=(diag(2)/5))
}
s2.red <- data.frame(s2.red)
colnames(s2.red) <- c('x','y')
s2.red$Class <- 'Red'

# Create matrix for Scenario Two
# Green class
s2.green <- matrix(nrow=100,ncol=2)
for (i in 1:100) {
  mean.1 <- sample(green.means[,1],1)
  mean.2 <- sample(green.means[,2],1)
  s2.green[i,]=mvrnorm(n=1,mu=c(mean.1,mean.2),Sigma=(diag(2)/5))
}
s2.green <- data.frame(s2.green)
colnames(s2.green) <- c('x','y')
s2.green$Class <- 'Green'

# Create Scenario Two data frame
scenario.2 <- rbind(s2.green,s2.red)

# Code to generate
plot.scenario.1 <- function(){
  p <- ggplot(scenario.1,aes(x,y))
  p <- p + geom_point(aes(colour=Class),pch=1,size=4)
  p <- p + scale_colour_manual(values=c('Green4','Red'))
  p <- p + theme_bw()
  return(p)
}

plot.scenario.2 <- function(){
  p <- ggplot(scenario.2,aes(x,y))
  p <- p + geom_point(aes(colour=Class),pch=1,size=4)
  p <- p + scale_colour_manual(values=c('Green4','Red'))
  p <- p + theme_bw()
  return(p)
}