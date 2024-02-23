rm(list=ls())
library(bootstrap)
n = nrow(law)
y = law$LSAT
z = law$GPA
cor.hat = cor(y,z)

# compute the jackknife replicates, leave-one-out estimates
cor.jack = numeric(n)
for(i in 1:n){
  cor.jack[i] = cor(y[-i],z[-i])
}
bias <- (n-1) * (mean(cor.jack) - cor.hat)
se = sqrt((n-1) * mean((cor.jack-cor.hat)^2))
print(bias)
print(se)