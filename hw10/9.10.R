set.seed(233333)

Gelman.Rubin <- function(psi){
  # psi[i,j] is the statistic psi(X[i,1:j])
  # for chain in i-th row of X
  psi <- as.matrix(psi)
  n <- ncol(psi)
  k <- nrow(psi)
  
  psi.means <- rowMeans(psi) # row means
  B <- n * var(psi.means)  # between variance est.
  psi.w <- apply(psi,1,'var') # within est.
  W <- mean(psi.w) # within est.
  v.hat <- W*(n-1)/n + (B/n) # upper variance est.
  r.hat <- v.hat/W # G-R statistic
  return(r.hat)
} 



Rayleigh.chain <- function(sigma=4,N=10000,X1=1){
  # generates a Metropolis chain for Rayleigh distribution
  
  f<-function(x,sigma){
    # calculate the density of Rayleigh distribution
    if (any(x<0)) return(0)
    stopifnot(sigma>0)
    return( (x/sigma^2) * exp(-x^2/(2*sigma^2)) )
  }
  
  x <- numeric(N)
  x[1] <- X1
  u <- runif(N)
  
  for(i in 2:N){
    xt <- x[i-1]
    y <- rchisq(1, df=xt)
    num <- f(y,sigma) * dchisq(xt, df=y)
    den <- f(xt,sigma) * dchisq(y, df=xt)
    if (u[i] <= num/den) 
      x[i] <- y 
    else 
      x[i] <- xt # y is rejected
  }
  return(x)
}

sigma <- 4 # parameter of Rayleigh distribution
k <- 4 # number of chains to generate
n <- 4000 # length of chains
b <- 1000 # burn-in length

# choose overdispersed initial values
x0 <- c(5,10,15,20)

# generate the chain
X <- matrix(0,nrow=k,ncol=n)
for(i in 1:k)
  X[i,] <- Rayleigh.chain(sigma,n,x0[i]) # each row is a chain

# compute the diagnostic statistics
psi <- t(apply(X,1,cumsum))
for(i in 1:nrow(psi)) 
  psi[i,] <- psi[i,] / (1:ncol(psi)) # cummulated mean
print(Gelman.Rubin(psi))

# plot the sequence of R-hat
rhat <- rep(0,n)
find.flag = FALSE
for(j in (b+1):n){
  rhat[j] <- Gelman.Rubin(psi[,1:j])
  if (rhat[j] < 1.2 && !find.flag){
    k = j # find the index of step where R is less than 1.2 for the first time
    find.flag = TRUE
  }
}
plot((b+1):n,rhat[(b+1):n],type='l',xlab='step',ylab='R.hat')
abline(h=1.2,v=k,lty=2,col='red')
print(k) 

# use the coda package to check for the convergence of the chain
library(coda)
X.chain.1 <- as.mcmc(X[1,])
X.chain.2 <- as.mcmc(X[2,])
X.chain.3 <- as.mcmc(X[3,])
X.chain.4 <- as.mcmc(X[4,])
X.chain <- mcmc.list(X.chain.1,X.chain.2,X.chain.3,X.chain.4)
gelman.diag(X.chain)

gelman.plot(X.chain,lwd=2)
