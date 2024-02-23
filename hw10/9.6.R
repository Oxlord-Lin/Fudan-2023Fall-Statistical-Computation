set.seed(2023)

# use the random walk Metropolis sampler with a uniform proposal chain
# theta should be within [0,1]
w <- 0.25  # width of the uniform support set
m <- 5000  # length of the chain
burn <- 1000  # burn-in time
x <- numeric(m)  # the chain

# the following function prob computes the target density
# (without the constant)

prob <- function(theta,animals){
  # computes (without the constant) the target density
  if (theta<0 || theta>1)
    return(0)
  ratio <- (1/2 + theta/4)^animals[1] * ((1-theta)/4)^animals[2] *
            ((1-theta)/4)^animals[3] * (theta/4)^animals[4]
}

# generate the rando walk Metropolis chain
animals <- c(125, 18, 20, 34)
u <- runif(m) # for accept/reject step
v <- runif(m,-w,w)  # proposal distribution
x[1] <- 0.5
for (i in 2:m){
  y <- x[i-1] + v[i]
  if (u[i]<= prob(y,animals)/prob(x[i-1],animals))
    x[i] <- y # accept
  else
    x[i] <- x[i-1] # reject
}

xb <- x[(burn+1):m]
print(mean(xb))

par(mfrow=c(2,1))
plot(x,type='l',col='royalblue')
hist(xb,prob=T,breaks = 'scott',xlab=bquote(theta),ylab='X',main='',col='aliceblue')
z <- seq(min(xb),max(xb),length=1000)
lines(z,dnorm(z,mean(xb),sd(xb)),col='royalblue')
abline(v=mean(xb),col='red',lwd=3,lty=3)