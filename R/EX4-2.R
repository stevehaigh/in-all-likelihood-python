
# Example 4.2
k<- 0:12
nk<- c(3, 24, 104, 286, 670, 1033, 1343,
      1112 , 829 , 478,  181 ,  45 ,   7)
  N<- sum(nk)
  pk <- nk/N

th<- sum(k*pk)/12
ihat <- sum(k*nk)/th^2 + sum((12-k)*nk)/(1-th)^2
  se <- sqrt(1/ihat)
cat('est=',th,'se=',se,'\n')


