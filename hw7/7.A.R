bootstrap <- function(x,statistic,B=200){
  n = length(x)
  theta.boot = numeric(n)
  for(b in 1:B){
    i = sample(1:n, size = n, replace = TRUE)
    mySample = x[i] 
    theta.boot[b] <- statistic(mySample)
  }
  return(theta.boot)
}

CI.standardNormal <- function(x,statistic,alpha=0.05,B=200){
  z = qnorm(1-alpha/2)
  theta.hat <- statistic(x)
  se = sd(bootstrap(x,statistic,B))
  return(c(theta.hat-z*se,theta.hat+z*se))
}

CI.basic <- function(x,statistic,alpha=0.05,B=200){
  theta.hat <- statistic(x)
  theta.boot <- bootstrap(x,statistic,B)
  Qt <- quantile(theta.boot,c(alpha/2, 1-alpha/2),type=1)
  names(Qt) <- rev(names(Qt))
  CI <- rev(2*theta.hat - Qt)
}

CI.percentile <- function(x,statistic,alpha=0.05,B=200){
  theta.boot <- bootstrap(x,statistic,B)
  Qt <- quantile(theta.boot,c(alpha/2, 1-alpha/2),type=1)
  return(Qt)
}

set.seed(2023)
result.standardNormal <- replicate(1000,expr={
  x = rnorm(5)
  CI <- CI.standardNormal(x,mean)
  coverage <- as.logical( CI[1] < 0 & 0 < CI[2] )
  miss.left <- as.logical( CI[1] > 0 )
  miss.right <- as.logical ( CI[2] < 0)
  c(CI,coverage,miss.left,miss.right) 
})
c(mean(result.standardNormal[3,]),mean(result.standardNormal[4,]),mean(result.standardNormal[5,])) # 0.849 0.069 0.082


set.seed(2023)
result.basic <- replicate(1000,expr={
  x = rnorm(5)
  CI <- CI.basic(x,mean)
  coverage <- as.logical( CI[1] < 0 & 0 < CI[2] )
  miss.left <- as.logical( CI[1] > 0 )
  miss.right <- as.logical ( CI[2] < 0)
  c(CI,coverage,miss.left,miss.right) 
})
c(mean(result.basic[3,]),mean(result.basic[4,]),mean(result.basic[5,])) # 0.842 0.076 0.082


set.seed(2023)
result.percentile <- replicate(1000,expr={
  x = rnorm(5)
  CI <- CI.percentile(x,mean)
  coverage <- as.logical( CI[1] < 0 & 0 < CI[2] )
  miss.left <- as.logical( CI[1] > 0 )
  miss.right <- as.logical ( CI[2] < 0)
  c(CI,coverage,miss.left,miss.right) 
})
c(mean(result.percentile[3,]),mean(result.percentile[4,]),mean(result.percentile[5,])) # 0.838 0.075 0.087


# combine all the results together in a dataframe
result <- as.data.frame(rbind(c(mean(result.standardNormal[3,]),mean(result.standardNormal[4,]),mean(result.standardNormal[5,])),c(mean(result.basic[3,]),mean(result.basic[4,]),mean(result.basic[5,])),c(mean(result.percentile[3,]),mean(result.percentile[4,]),mean(result.percentile[5,]))))
colnames(result) <- c('coverage rate','missing left','missing right')
rownames(result) <- c('standard normal','basic','percentile')
result
#                 coverage rate missing left missing right
# standard normal         0.849        0.069         0.082
# basic                   0.842        0.076         0.082
# percentile              0.838        0.075         0.087