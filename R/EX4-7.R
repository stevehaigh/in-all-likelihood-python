# Example 4.7
 
x<- c(447,132,42,21,3,2)
kk<- 0:4
n<- sum(x)

# Poisson fit
  lambda <- sum(x*c(0:5))/n; cat('lambda-hat = ',lambda,'\n')
  phat <- dpois(0:5,lambda)
  fhat <- n*phat
  print(cbind(Obs=x,'E-Pois'=round(fhat,1)))
chi2<- sum((x-fhat)^2/fhat)
  cat('Chi2 goodness-of-fit = ',chi2,'\n')
