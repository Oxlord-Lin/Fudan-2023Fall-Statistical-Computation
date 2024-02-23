Inv_trans <- function(n){
  rs = runif(n);
  for(i in 1:n){
    temp = rs[i]
    if (temp <= 0.1){
      rs[i] = 0;
    }else if(temp <= 0.3){
      rs[i] = 1;
    }else if(temp <= 0.5){
      rs[i] = 2;
    }else if(temp <= 0.7){
      rs[i] = 3;
    }else{
      rs[i] = 4;
    }
  }
  rs
}

RVs = Inv_trans(1000);
mySize = 400;
theoretical = c(.1, .2, .2, .2, .3)
T = matrix(rep(0,5*5),5,5)
for (i in 1:5){
  mySample = sample(RVs,size=mySize, replace=F)
  empirical = table(mySample)/mySize
  T[i,] = empirical
}
T = rbind(theoretical,T)
rownames(T) = c('theoretical prob','empirical prob 1',
                'empirical prob 2','empirical prob 3',
                'empirical prob 4','empirical prob 5')
colnames(T) = c(0,1,2,3,4)
print(T)
write.csv(T,file='3.5_result.csv')
