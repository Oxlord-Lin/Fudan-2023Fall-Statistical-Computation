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
  Qt <- quantile(theta.boot,c(alpha/2, 1-alpha/2),type=1,na.rm = TRUE)
  names(Qt) <- rev(names(Qt))
  CI <- rev(2*theta.hat - Qt)
}

CI.percentile <- function(x,statistic,alpha=0.05,B=200){
  theta.boot <- bootstrap(x,statistic,B)
  Qt <- quantile(theta.boot,c(alpha/2, 1-alpha/2),type=1,na.rm=TRUE)
  return(Qt)
}

sk <- function(x){
  # compute the sample skewness coeff
  xbar <- mean(x)
  m3 <- mean((x-xbar)^3)
  m2 <- mean((x-xbar)^2)
  # if(is.na(m3/m2^1.5))
  #   {print('error')}
  return(m3/m2^1.5)
}


# the following is the nornmal distribution
set.seed(2023)
result.standardNormal <- replicate(1000,expr={
  x = rnorm(5)
  CI <- CI.standardNormal(x,sk)
  sk.hat <- sk(x) # sample skewness statistic
  coverage <-  CI[1] < 0 & 0 < CI[2] 
  miss.left <-  CI[1] > 0
  miss.right <-  CI[2] < 0
  c(CI,coverage,miss.left,miss.right) 
})
proportion1 <- c(mean(result.standardNormal[3,],na.rm = TRUE),mean(result.standardNormal[4,],na.rm = TRUE),mean(result.standardNormal[5,],na.rm = TRUE))


set.seed(2023)
result.basic <- replicate(1000,expr={
  x = rnorm(5)
  CI <- CI.basic(x,sk)
  sk.hat <- sk(x) # sample skewness statistic
  coverage <-  CI[1] < 0 & 0 < CI[2] 
  miss.left <-  CI[1] > 0
  miss.right <-  CI[2] < 0
  c(CI,coverage,miss.left,miss.right) 
})
proportion2 <- c(mean(result.basic[3,]),mean(result.basic[4,]),mean(result.basic[5,])) 


set.seed(2023)
result.percentile <- replicate(1000,expr={
  x = rnorm(5)
  sk.hat <- sk(x)# sample skewness statistic
  CI <- CI.percentile(x,sk)
  coverage <-  CI[1] < 0 & 0 < CI[2] 
  miss.left <-  CI[1] > 0 
  miss.right <-  CI[2] < 0
  c(CI,coverage,miss.left,miss.right) 
})
proportion3 <-c(mean(result.percentile[3,]),mean(result.percentile[4,]),mean(result.percentile[5,]))


# combine all the results together in a dataframe
result <- as.data.frame(rbind(proportion1,proportion2,proportion3))
colnames(result) <- c('coverage rate','missing left','missing right')
rownames(result) <- c('standard normal','basic','percentile')
result



# the following is the chi-square distribution
set.seed(2023)
result.standardNormal <- replicate(1000,expr={
  x = rchisq(5,df=5)
  CI <- CI.standardNormal(x,sk)
  sk.hat <- sk(x)
  coverage <-  CI[1] < sqrt(8/5) & sqrt(8/5) < CI[2] 
  miss.left <-  CI[1] > sqrt(8/5) 
  miss.right <-  CI[2] < sqrt(8/5)
  c(CI,coverage,miss.left,miss.right) 
})
proportion1 <- c(mean(result.standardNormal[3,],na.rm = TRUE),mean(result.standardNormal[4,],na.rm = TRUE),mean(result.standardNormal[5,],na.rm = TRUE))


set.seed(2023)
result.basic <- replicate(1000,expr={
  x = rchisq(5,df=5)
  CI <- CI.basic(x,sk)
  sk.hat <- sk(x)
  coverage <-  CI[1] < sqrt(8/5) & sqrt(8/5) < CI[2] 
  miss.left <-  CI[1] > sqrt(8/5)
  miss.right <-  CI[2] < sqrt(8/5)
  c(CI,coverage,miss.left,miss.right) 
})
proportion2 <- c(mean(result.basic[3,]),mean(result.basic[4,]),mean(result.basic[5,])) 


set.seed(2023)
result.percentile <- replicate(1000,expr={
  x = rchisq(5,df=5)
  sk.hat <- sk(x)
  CI <- CI.percentile(x,sk)
  coverage <-  CI[1] < sqrt(8/5) & sqrt(8/5) < CI[2] 
  miss.left <-  CI[1] > sqrt(8/5) 
  miss.right <-  CI[2] < sqrt(8/5)
  c(CI,coverage,miss.left,miss.right) 
})
proportion3 <-c(mean(result.percentile[3,]),mean(result.percentile[4,]),mean(result.percentile[5,]))


# combine all the results together in a dataframe
result <- as.data.frame(rbind(proportion1,proportion2,proportion3))
colnames(result) <- c('coverage rate','missing left','missing right')
rownames(result) <- c('standard normal','basic','percentile')
result
