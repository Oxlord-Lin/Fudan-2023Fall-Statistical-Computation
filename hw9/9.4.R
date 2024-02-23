# the following function evaluate the Laplace density
f <- function(x){
  return( 0.5 * exp(-abs(x)) )
}

# the following function is the random walk Metropolis sampler
RandomWalk.Metropolis <- function(sigma,x0,N){
  x <- numeric(N)
  x[1]  <- x0
  u = runif(N)
  k <- 0
  for(i in 2:N){
    y <- rnorm(1,x[i-1],sigma) # proposal by normal distribution
    if(u[i] <= f(y)/f(x[i-1])){
      x[i] <- y # accepted
    }
    else{ # rejected
      x[i] <- x[i-1]
      k <- k + 1
    }
  }
  return(list(x=x,k=k))
}

# compare the chain and acceptance rates using different variances
N <- 5000
sigma <- c(.05, .5, 2, 16)
x0 <- 0
set.seed(2023)
rw1 <- RandomWalk.Metropolis(sigma[1],x0,N)
rw2 <- RandomWalk.Metropolis(sigma[2],x0,N)
rw3 <- RandomWalk.Metropolis(sigma[3],x0,N)
rw4 <- RandomWalk.Metropolis(sigma[4],x0,N)

# compare the chain
par(mfrow=c(2,2))
rw <- cbind(rw1$x,rw2$x,rw3$x,rw4$x)
for(j in 1:4){
  plot(rw[,j],type='l',
  xlab=bquote(sigma == .(round(sigma[j],3))),
  ylab='X',ylim=range(rw[,j]) )
}

# compare the histogram
par(mfrow=c(2,2))
xx <- seq(-10,10,by=0.01)
yy <- f(xx)
for(j in 1:4){
  hist(rw[,j],breaks='Scott',freq = F,ylim = c(0,0.5),
       xlab=bquote(sigma == .(round(sigma[j],3))),main='Histogram')
  lines(xx,yy,col='red',lwd=1.5)
}

# compare the acceptance rates, the suitable interval is [0.15,0.5]
print(c(rw1$k,rw2$k,rw3$k,rw4$k)/N)
