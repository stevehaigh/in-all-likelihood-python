
# Example 4.3
k<- 0:12
nk<- c(3, 24, 104, 286, 670, 1033, 1343,
      1112 , 829 , 478,  181 ,  45 ,   7)
  N<- sum(nk)
  pk <- nk/N
th<- sum(k*pk)/12

#s2 <- sum((pk*(k-12*th)^2))  
#  cat(s2,12*th*(1-th),'\n')  # observed variance vs binomial variance

# residuals:
est <- N*dbinom(k,12,th)     # expected frequencies

chi2 <- sum((nk-est)^2/est)
  cat(chi2,(1-pnorm(chi2,11)),'\n')

res <- (nk-est)/sqrt(est)
  print(cbind(Obs=nk,Exp=round(est),Residual=round(res,1)))


