trimed.means <- function(x,k){
  x <- sort(x)
  return( sum(x[(k+1):(n-k)])/(n-2*k) )
}


m <- 100 # replicating times
n <- 20 # sample size
set.seed(123)
smp <- matrix(rcauchy(n*m),nrow=n)
mse <- numeric(9)
for(k in 1:9){
  tmeans <- numeric(m)
  for(j in 1:m){
    tmeans[j] <-trimed.means(smp[,j],k) 
  }
  mse[k] = mean((tmeans-mean(tmeans))^2)
}

k = 1:9
rbind(k,mse)
plot(k,mse,type='l',main='MSE of the k-th trimmed means for standard Cauchy')
