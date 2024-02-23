rm(list=ls())

# 需要补充一些MLE的推导过程
hazard_rate = function(x){
  lambda = length(x)/sum(x)
  return(lambda)
}


data(aircondit,package = 'boot')
B = 200
n = nrow(aircondit)
lambda.hat = hazard_rate(aircondit) # 根据原始样本计算得到的lambda
lambda.bootstrap = numeric(B)

# bootstrap estimate of lambda
set.seed(2023)
for(b in 1:B){
  index = sample(1:n,size=n,replace = T)
  mySample = aircondit[index,1]
  lambda.bootstrap[b] = hazard_rate(mySample)
}

# bias
print(bias.lambda <- mean(lambda.bootstrap-lambda.hat))
# se
print(se.lambda <- sd(lambda.bootstrap))
