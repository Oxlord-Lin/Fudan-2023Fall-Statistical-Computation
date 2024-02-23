library(bootstrap)
boot.t.ci <- 
  function(x,B=500,R=100,level=0.95,statistic){
    # compute the bootstrap t CI
    x <- as.matrix(x)
    n <- nrow(x)
    stat.outerLoop <- numeric(B)
    se <- numeric(B) # se for the inner loop
    
    boot.se <- function(x,R,f){ # inner loop
      # local function to compute the bootstrap
      # estimate of standard error for statistic f(x)
      x <- as.matrix(x)
      m = nrow(x)
      stat.innerLoop <- replicate(R,expr={
        i <- sample(1:m, size = m, replace = TRUE)
        f(x[i,])
      })
      return(sd(stat.innerLoop))
    }
    
    for(b in 1:B) { # outer loop
      j <- sample(1:n,size = n, replace = TRUE)
      y <- x[j,]
      stat.outerLoop[b] <- statistic(y)
      se[b] = boot.se(y,R=R,f = statistic)
    } 
    stat0 <- statistic(x)
    t.stats <- (stat.outerLoop - stat0)/se
    se0 <- sd(stat.outerLoop)
    alpha <- 1 - level
    Qt <- quantile(t.stats,c(alpha/2, 1-alpha/2),type=1)
    names(Qt) <- rev(names(Qt))
    CI <- rev(stat0 - Qt * se0)
    return(CI)
  }

set.seed(23333)
dat <- law
stat <- function(dat){
  cor(dat[,1],dat[,2])
}
CI <- boot.t.ci(dat,B=2000,R=200,statistic=stat)
print(CI)
#     2.5%        97.5% 
#   -0.2858143  0.9869492 
