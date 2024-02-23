fe <- function(n){
  U1 = -1 + 2*runif(n)
  U2 = -1 + 2*runif(n)
  U3 = -1 + 2*runif(n)
  for(i in 1:n){
    if (abs(U3[i])>=abs(U2[i]) && abs(U3[i])>=abs(U1[i])){
      U3[i] = U2[i]
    }
  }
  U3
}

hist(fe(10000),col='orange',breaks=20,freq = F,xlab = 'x',ylab = 'density estimate',main='histogram density estimate by 10000 samples ')
xx = seq(-1,1,length = 1000)
yy = 0.75*(1-xx^2)
lines(xx,yy,col='blue',lwd=2)

