library(DAAG)
attach(ironslag)
n <- length(magnetic)
e1 <- e2 <- e3 <- e4 <- numeric(n)

# selecting by n-fold cross validation
for(k in 1:n){
  y = magnetic[-k]
  x = chemical[-k]
  
  J1 <- lm(y~x)
  yhat1 <- J1$coefficients[1] + J1$coefficients[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1
  
  J2 <- lm(y~x+I(x^2))
  yhat2 <- J2$coefficients[1] + J2$coefficients[2] * chemical[k] + J2$coefficients[3]*chemical[k]^2
  e2[k] <- magnetic[k] - yhat2
  
  J3 <- lm(log(y)~x)
  logyhat3 <- J3$coefficients[1] + J3$coefficients[2] * chemical[k]
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3
  
  J4 <- lm(y~x+I(x^2)+I(x^3))
  yhat4 <- J4$coefficients[1] + J4$coefficients[2] * chemical[k] + J4$coefficients[3]*chemical[k]^2 + J4$coefficients[4]*chemical[k]^3
  e4[k] <- magnetic[k] - yhat4
}
c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2))
# 19.55644 17.85248 18.44188 18.17756
# the quadratic model is the best fitting model by cross validation

# selecting by R^2
y = magnetic
x = chemical

J1 <- lm(y~x)
J1 <- summary(J1)

J2 <- lm(y~x+I(x^2))
J2 <- summary(J2)

J3 <- lm(log(y)~x)
J3 <- summary(J3)

J4 <- lm(y~x+I(x^2)+I(x^3))
J4 <- summary(J4)

print(c(J1$adj.r.squared,J2$adj.r.squared,J3$adj.r.squared,J4$adj.r.squared))
# 0.5281545 0.5768151 0.5280556 0.5740396
# the quadratic model is still the best fitting model by maximum adjusted R^2
