# Example 17.11

x<- matrix(scan('seed.dat'),ncol=2,byrow=T)
  y<-x[,1]  # number of successes
  n<-x[,2]

seed<- c(rep(0,11),rep(1,10))
extract<- c(rep(0,5),rep(1,6),rep(0,5),rep(1,5))

reg<-glm(cbind(y,n-y)~ seed*extract,family=binomial)
regsum<- summary(reg)

N<- length(y)
X<- matrix(0,nrow=N,ncol=4)
  X[,1]<- 1
  X[,2]<- seed
  X[,3]<- extract
  X[,4]<- seed*extract

# starting values
beta<- reg$coef
b<- 0
s2b<- .24^2  

Yfun<- function(beta,b){
  eta <- X%*% beta + b 
  p<- exp(eta)/ (1+ exp(eta))
  err<- (y-n*p)/(n*p*(1-p))
  Y <- eta + err
  wt <- n*p*(1-p)
  return(Y=c(Y),wt=c(wt))
}

LSIG<- NULL #log-likelihood of s2b
SIG<- NULL
for (i in 1:10){

# step 1
  a<- Yfun(beta,b);  Y<- a$Y; wt<- a$wt
  b <- wt*(Y- c(X%*%beta))/(wt + 1/s2b)
# step 2
  #a<- Yfun(beta,b);  Y<- a$Y; wt<- a$wt
  XWX<- t(X) %*% (wt*X)
  XWY<- t(X) %*% (wt*(Y-b))
  beta<- solve(XWX,XWY)
# step 3
  s2b <- mean(b^2) + mean(1/(wt+1/s2b))

# profile likelihood
  lsig <- -1/2*sum(log(wt)) + sum(wt*(Y-c(X%*%beta)-b)^2)/2 +
         N/2*log(s2b) + 1/2/s2b*sum(b^2) + 1/2*sum(log(wt+1/s2b))
  LSIG<- c(LSIG,lsig)
  SIG <- c(SIG,sqrt(s2b))
cat(i,round(c(beta,sqrt(s2b)),2),'\n')

}

# normality of random effects
  nn<- c(5,6,5,5); NN<- c(0,cumsum(nn))
  group<- rep(c(1,2,3,4),nn)
  group<- factor(group)
  varb<- tapply(b,group,var)
  a<- qqnorm(b,plot=F)
  plot(a,type='n')
  text(a$x,a$y,c(group))

# getting standard errors:
V <- 1/wt + s2b
XVX <- t(X) %*% (1/V * X)
vars<- diag(solve(XVX))
cat('Std errors of betahat=',round(sqrt(vars),2),'\n')

