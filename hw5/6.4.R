m <- 1000 # constructing the CI for m times
n <- 1000 # sample size

set.seed(123)
miu.true = 1
smp = matrix(rlnorm(n*m,meanlog = miu.true),nrow = n)
smp.log = log(smp)
miu = apply(smp.log,2,mean)
sd = apply(smp.log,2,sd)
se = sd/sqrt(n)
z = qnorm(0.975)
# construct an approximately 95% CI for miu
CI.low = miu - z*se
CI.high = miu + z*se
CI = cbind(CI.low,CI.high)

# find the empirical confidence level
confidence_level <- sum(CI.low<miu.true & miu.true<CI.high)/m 
print(confidence_level) # 0.961

