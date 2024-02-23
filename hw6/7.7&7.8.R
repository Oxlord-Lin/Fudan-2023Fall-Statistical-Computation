rm(list=ls())
library(bootstrap)

theta <- function(data){
  Sigma <- ((n-1)/n) * cov(data)
  eigen.values = eigen(Sigma)$values
  return(eigen.values[1]/sum(eigen.values))
}

set.seed(2023)
B = 200
n = nrow(scor)
theta.hat = theta(scor) # 根据原始样本计算得到的theta
theta.bootstrap = numeric(B)

# bootstrap
for(b in 1:B){
  index = sample(1:n,size=n,replace = T)
  mySample = scor[index,]
  theta.bootstrap[b] = theta(mySample)
}

# bias
print(bias.theta.bootstrap <- mean(theta.bootstrap-theta.hat))
# se
print(se.theta.bootstrap <- sd(theta.bootstrap))


# jackknife
# compute the jackknife replicates, leave-one-out estimates
theta.jack = numeric(n)
for(i in 1:n){
  theta.jack[i] = theta(scor[-i,])
}

# bias
bias.theta.jackknife <- (n-1) * (mean(theta.jack) - theta.hat)
print(bias.theta.jackknife)
# se
se.theta.jackknife = sqrt((n-1) * mean((theta.jack-theta.hat)^2))
print(se.theta.jackknife)
