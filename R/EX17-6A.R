# Example 17.6


x<- matrix(scan('seed.dat'),ncol=2,byrow=T)
  y<-x[,1]  # number of successes
  n<-x[,2]

seed<- c(rep(0,11),rep(1,10))
extract<- c(rep(0,5),rep(1,6),rep(0,5),rep(1,5))

reg<-glm(cbind(y,n-y)~ seed*extract,family=binomial)
  regsum<- summary(reg)
  cat('Standard logistic regression\n')
  print(round(regsum$coef,2))


# ...........  GLMM

N<- length(y)
X<- matrix(0,nrow=N,ncol=4)
  X[,1]<- 1
  X[,2]<- seed
  X[,3]<- extract
  X[,4]<- seed*extract

# ............  exact computation, at fixed betahat

source('binormal.r')           # binomial normal marginal
ELIKE<- function(param){   # Exact minus log-likelihood
  beta<- param[1:4]
  sig<- param[5]
  elike<- 0
  eta <- X%*% beta
  p<- exp(eta)/ (1+ exp(eta))
  for (ii in 1:N){
     elike <- elike - log(dbinorm(y[ii],n[ii],p[ii],sig))
  }
}

beta0<- c(-.56,.15,1.32,-.78)  # starting value from glm
a<-nlm(ELIKE,p=c(beta0,0.05),hessian=T)

cat('Estimate=',round(a$est,2),'\n')
varmat<- solve(a$hessian)
se<- sqrt(diag(varmat))
cat("SE's =",round(se,3),'\n')

# computing profile likelihood for sigma
ELIKE0<- function(param){
  beta<- param
  elike<- 0
  eta <- X%*% beta
  p<- exp(eta)/ (1+ exp(eta))
  for (ii in 1:N){
     elike <- elike - log(dbinorm(y[ii],n[ii],p[ii],sig))
  }
}

# this will be very slow!!
beta0<- a$est[1:4]   # MLE for starting value
SIG <- seq(.001,.6,len=20)

PROF<-NULL
for (sig in SIG){   
  a<-nlm(ELIKE0,p=beta0,hessian=T)
  PROF<- c(PROF,a$min)
}
PROF<- min(PROF)-PROF
plot(SIG,PROF,type='l')

