G <- function(x.raw){ # 计算基尼系数
  x = sort(x.raw) # turn x.raw into ordering statistics
  n = length(x)
  miu = mean(x)
  G = (1/(n^2*miu)) * sum((2*(1:n)-n-1)*x)
  return(G)
}

m = 1000 # repeating times
n = 1000 # sample size

# Gini ratio for lognormal distribution
set.seed(233)
smp = matrix(rlnorm(m*n),nrow=n)
G.lognormal = apply(smp,2,G)
G.lognormal.mean = mean(G.lognormal)
G.lognormal.median = median(G.lognormal)
G.lognormal.deciles = quantile(G.lognormal, probs = seq(.1, .9, by = .1))
hist(G.lognormal, freq = FALSE, main = "Density histograms of the Ginis ratio estimates of standard lognormal") # Create a histogram

# Gini ratio for uniform distribution
set.seed(233)
smp = matrix(runif(m*n),nrow=n)
G.uniform = apply(smp,2,G)
G.uniform.mean = mean(G.uniform)
G.uniform.median = median(G.uniform)
G.uniform.deciles = quantile(G.uniform, probs = seq(.1, .9, by = .1))
hist(G.uniform, freq = FALSE, main = "Density histograms of the Ginis ratio estimates of uniform distribution") # Create a histogram

# Gini ratio for Bernoulli distribution
set.seed(233)
smp = matrix(rbinom(m*n,size=1,prob=0.1),nrow=n)
G.Bernoulli = apply(smp,2,G)
G.Bernoulli.mean = mean(G.Bernoulli)
G.Bernoulli.median = median(G.Bernoulli)
G.Bernoulli.deciles = quantile(G.Bernoulli, probs = seq(.1, .9, by = .1))
hist(G.Bernoulli, freq = FALSE, main = "Density histograms of the Ginis ratio estimates of Bernoulli(0.1)") # Create a histogram

# CI for E[G]
sigma.true = 5
G.true = 2*pnorm(sigma.true/sqrt(2)) - 1 # from wikipedia
set.seed(233)
# sampling
smp = matrix(rlnorm(m*n,meanlog=0,sdlog=sigma.true),nrow=n)
smp.log = log(smp)
# calculate the sample variance for each column
S2 = apply(smp.log,2,var)
# construct a 95% CI for sigma
sigma.CI.low = numeric(m)
sigma.CI.high = sqrt((n-1)*S2/qchisq(.05,df=n-1))
sigma.CI = cbind(sigma.CI.low,sigma.CI.high)
# construct a 95% CI for G.true
G.CI.low = numeric(m)
G.CI.high = 2*pnorm(sigma.CI.high/sqrt(2))-1
G.CI = cbind(G.CI.low,G.CI.high)

confidence_level = sum(G.CI.low<G.true & G.true<G.CI.high)/m
print(confidence_level) #  0.951
