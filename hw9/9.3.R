set.seed(123)

m <- 10000
x <- numeric(m)
x[1] <- rt(1,df=10) # using t distribution as proposal
k <- 0
u <- runif(m)

for(i in 2:m){
  xt <- x[i-1]
  y <- rt(1,df=abs(xt)+1) # proposal by t distribution
  num <- dcauchy(y) * dt(xt,df=abs(y)+1)
  den <- dcauchy(xt) * dt(y,df=abs(xt)+1)
  if(u[i] <= num/den){ # accepted 
    x[i] <- y 
  } 
  else{ # rejected
    x[i] <- xt
    k <- k+1
  }
}

index <- 1001:m
x1 <- x[index]

deciles.MC <- quantile(x1, probs=seq(0.1,0.9, by=0.1))
deciles.standard <- qcauchy(seq(0.1,0.9, by=0.1))
comparison = as.data.frame(rbind(deciles.standard,deciles.MC))
rownames(comparison) <- c('standard','MC')
print(comparison)
