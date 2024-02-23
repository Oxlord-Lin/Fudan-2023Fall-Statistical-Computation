# The following function evaluates the Rayleight density
f <- function(x,sigma=2){
  if (any(x<0)) return(0)
  stopifnot(sigma > 0)
  return((x / sigma^2) * exp( -x^2 / (2*sigma^2) ) ) 
}

par(mfrow=c(2,1))
m <- 10000

for(sigma in c(2,4)){
  set.seed(123456)
  x <- numeric(m)
  x[1] <- rchisq(1,df=1)
  k <- 0
  u <- runif(m)
  
  for(i in 2:m){
    xt <- x[i-1]
    y <- rchisq(1,df=xt) # proposal
    num <- f(y,sigma) * dchisq(xt,df=y)
    den <- f(xt,sigma) * dchisq(y,df=xt)
    if(u[i] <= num/den){ # accepted 
      x[i] <- y 
      } 
    else{ # rejected
      x[i] <- xt
      k <- k+1
    }
  }
  print(k/m)
  
  index <- 5000:5500
  x1 <- x[index]
  plot(index,x1,type='l',main=paste('sigma=',sigma),ylab='x')
}