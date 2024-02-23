set.seed(2023)

# initialize constants and parameters
N <- 5000
burn <- 1000
X <- matrix(0,N,2)

rho <- .9
mu2 <- mu1 <- 0
sigma2 <- sigma1 <- 1
s2 <- s1 <- sqrt(1-rho^2)*sigma1

# generate the chain
X[1,] <- c(mu1,mu2) # initialize
for ( i in 2:N){
  x2 <- X[i-1,2]
  m1 <- mu1 + rho*(x2 - mu2)*sigma1/sigma2
  X[i,1] <- rnorm(1,m1,s1)
  x1 <- X[i,1]
  m2 <- mu2 + rho*(x1 - mu1) * sigma2/sigma1
  X[i,2] <- rnorm(1,m2,s2)
}

b <- burn + 1
x <- X[b:N,]
plot(x,main='',cex=.5, xlab=bquote(X[1]),ylab=bquote(X[2]),xlim=range(x[,1]),ylim=range(x[,2]))

x.dataframe <- as.data.frame(x)
colnames(x.dataframe) <- c('Y','X')
model <- lm(Y~.,x.dataframe)


par(mfrow=c(1,2))
qqnorm(model$residuals)
qqline(model$residuals,col='red')
plot(model$fitted.values,model$residuals,main='Residual vs Fitted values')
par(mfrow=c(1,1))
