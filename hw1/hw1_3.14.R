rmvn.Choleski <-
  function(n, mu, Sigma) {
    # generate n random vectors from MVN(mu, Sigma)
    # dimension is inferred from mu and Sigma
    d <- length(mu)
    Q <- chol(Sigma) # Choleski factorization of Sigma
    Z <- matrix(rnorm(n*d), nrow=n, ncol=d)
    X <- Z %*% Q + matrix(mu, n, d, byrow=TRUE)
    X
  }

Sigma = c(1,-.5,.5,-.5,1,-.5,.5,-.5,1)
Sigma = matrix(Sigma,nrow=3)
miu = c(0,1,2)
samples = rmvn.Choleski(200,miu,Sigma)

X1 = samples[,1]
X2 = samples[,2]
X3 = samples[,3]
pairs(samples)
apply(samples,MARGIN = 2,FUN = mean)
cor(X1,X2)
cor(X1,X3)
cor(X2,X3)


