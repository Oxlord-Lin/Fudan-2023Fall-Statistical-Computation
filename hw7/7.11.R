library(DAAG)
attach(ironslag)
n <- length(magnetic)
e1 <- e2 <- e3 <- e4 <- numeric(n*(n-1)/2)

# selecting by leave-two-out cross validation
k = 1
for(i in 1:(n-1))
  for(j in (i+1):n){
    # leave two samples out
    y = magnetic[c(-i,-j)] 
    x = chemical[c(-i,-j)]
    
    J1 <- lm(y~x)
    yhat1.i <- J1$coefficients[1] + J1$coefficients[2] * chemical[i]
    e1[k] <- magnetic[i] - yhat1.i
    if (is.na(e1[k])){
      print('error!')
    }
    yhat1.j <- J1$coefficients[1] + J1$coefficients[2] * chemical[j]
    e1[k+1] <- magnetic[j] - yhat1.j
    if (is.na(e1[k+1])){
      print('error!')
    }
    
    J2 <- lm(y~x+I(x^2))
    yhat2.i <- J2$coefficients[1] + J2$coefficients[2] * chemical[i] + J2$coefficients[3]*chemical[i]^2
    e2[k] <- magnetic[i] - yhat2.i
    yhat2.j <- J2$coefficients[1] + J2$coefficients[2] * chemical[j] + J2$coefficients[3]*chemical[j]^2
    e2[k+1] <- magnetic[j] - yhat2.j
    
    J3 <- lm(log(y)~x)
    logyhat3.i <- J3$coefficients[1] + J3$coefficients[2] * chemical[i]
    yhat3.i <- exp(logyhat3.i)
    e3[k] <- magnetic[i] - yhat3.i
    logyhat3.j <- J3$coefficients[1] + J3$coefficients[2] * chemical[j]
    yhat3.j <- exp(logyhat3.j)
    e3[k+1] <- magnetic[j] - yhat3.j
    
    J4 <- lm(log(y)~log(x))
    logyhat4.i <- J4$coefficients[1] + J4$coefficients[2] * log(chemical[i])
    yhat4.i <- exp(logyhat4.i)
    e4[k] <- magnetic[i] - yhat4.i
    logyhat4.j <- J4$coefficients[1] + J4$coefficients[2] * log(chemical[j])
    yhat4.j <- exp(logyhat4.j)
    e4[k+1] <- magnetic[i] - yhat4.j
    
    k <- k + 2
}
c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2))
# 19.57227 17.87018 18.45491 26.00865
# the quadratic model is still the best fitting model by leave-two-out cross validatioin