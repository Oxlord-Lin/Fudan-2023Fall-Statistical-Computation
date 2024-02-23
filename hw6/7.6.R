rm(list=ls())
library(bootstrap)
# Correlation panel and the correlation matrix
# pairs(scor,lower.panel = NULL)
# cor(scor)

library(psych)
pairs.panels(scor, 
             pch=19,
             method = "pearson", # correlation method
             hist.col = "wheat",
             density = TRUE,  # show density plots
             ellipses = F # show correlation ellipses
)


# bootstrap estimate of se
set.seed(2023)
se.bootstrap <- function(data){
  B = 200
  n = nrow(data)
  cor.bootstrap = numeric(B)
  for(b in 1:B){
    index = sample(1:n,size=n,replace = T)
    mySample = data[index,]
    cor.bootstrap[b] = cor(mySample[,1],mySample[,2])
  }
  return(sd(cor.bootstrap))
}

se.cor12 <- se.bootstrap(scor[,c(1,2)])
se.cor34 <- se.bootstrap(scor[,c(3,4)])
se.cor35 <- se.bootstrap(scor[,c(3,5)])
se.cor45 <- se.bootstrap(scor[,c(4,5)])
print(c(se.cor12,se.cor34,se.cor35,se.cor45))