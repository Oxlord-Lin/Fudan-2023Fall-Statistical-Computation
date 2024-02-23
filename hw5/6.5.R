m = 10000 # repeating times
n = 20 # sample size
set.seed(233)
smp = matrix(rchisq(n*m,df=2),nrow = n)
means = apply(smp,2,mean)
sd = apply(smp,2,sd)
se = sd/sqrt(n)
z = qt(0.975,df=n-1)
CI.low = means - z*se
CI.high =  means + z*se
CI = cbind(CI.low,CI.high)

coverage_prob = sum(CI.low<2 & 2<CI.high)/m
print(coverage_prob) # 0.9161

# comparison: more robust than variance! 
# not too far away from 0.95