set.seed(2023)

chain <- function(n,a,b,x,y,N){

  X <- matrix(0,N,2) # to store the chain

  # generate the chain
  X[1,] <- c(1,0.5) # initialize the chain 
  for (i in 2:N){
    x2 <- X[i-1,2]
    X[i,1] <- rbinom(1,n,y)
    # print(y)
    x1 <- X[i,1]
    X[i,2] <- rbeta(1,shape1=x+a, shape2=n-x+b)
  }

  return(X)
}

burn <- 1000
N <- 5000 # length of the chain
n <- 20
a <- 10
b <- 10

X <- chain(n,a,b,x=10,y=0.5,N)

Xb <- X[(burn + 1):N,] # drop the burn-in time samples

x <- Xb[,1]
y <- Xb[,2]

par(mfrow=c(1,1))
plot(Xb,cex=.5,type = 'p',xlab=bquote(x),ylab=bquote(y),
     col='navyblue',main='scatter plot of the joint density f(x,y)')

par(mfrow=c(2,1))
plot(x,type='l',col='royalblue',main='Chain of x')
hist(x,probability = T,col='gold')
z <- seq(min(x),max(x),length=1000)
lines(z,dnorm(z,mean=mean(x),sd=sd(x)))

plot(y,type='l',col='royalblue',main='Chain of y')
hist(y,probability = T,breaks='scott',col='gold')
z <- seq(min(y),max(y),length=1000)
lines(z,dnorm(z,mean=mean(y),sd=sd(y)))

par(mfrow=c(1,1))
