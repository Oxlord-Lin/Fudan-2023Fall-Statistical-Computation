Exp_Gamma_mixture <- function(r,beta,n){
  U1 = rgamma(n,r,beta)
  U2 = rexp(n,U1)
}

r = 4
beta = 2
results = Exp_Gamma_mixture(r,beta,1000)
hist(results,col='orange',probability = T,breaks=30,xlab='x',ylab = 'density estimate',
     main='density histogram of the sample from Exponential-Gamma mixture and Pareto density curve')
xx = seq(min(results),max(results),length=1000)
yy = beta^r * r * (beta+xx)^(-r-1)
lines(xx,yy,col='blue',lwd=2)

