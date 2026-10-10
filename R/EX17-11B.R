# Exercise 18.11: Figure 17.4

x<- matrix(scan('seed.dat'),ncol=2,byrow=T)
  y<-x[,1]  # success
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

Yfun<- function(beta,b){
  eta <- X%*% beta + b 
  p<- exp(eta)/ (1+ exp(eta))
  err<- (y-n*p)/(n*p*(1-p))
  Y <- eta + err
  wt <- n*p*(1-p)
  return(Y=c(Y),wt=c(wt))
}

LSIG<- NULL #log-likelihood of s2b
LSIG2<-NULL
SIG<- seq(.01,.6,len=40)
beta<- reg$coef; # starting value for iteration
b<- 0

for (s2b in SIG^2){

nrep<- ifelse (s2b==SIG[1]^2,20,5)
for (i in 1:nrep){
# step 1
  a<- Yfun(beta,b);  Y<- a$Y; wt<- a$wt
  b <- wt*(Y- c(X%*%beta))/(wt + 1/s2b)
# step 2
  #a<- Yfun(beta,b);  Y<- a$Y; wt<- a$wt
  XWX<- t(X) %*% (wt*X)
  XWY<- t(X) %*% (wt*(Y-b))
  beta<- solve(XWX,XWY)
}
# profile likelihood
  lsig <- 1/2*sum(log(wt)) - 1/2*sum(wt*(Y-c(X%*%beta)-b)^2) -
          N/2*log(s2b) - 1/2/s2b*sum(b^2) - 1/2*sum(log(wt+1/s2b))
  LSIG2 <- c(LSIG2,lsig)  # quad-approx

  eta <- X%*% beta + b 
  p<- exp(eta)/ (1+ exp(eta))
  lsig <- sum(y*log(p) + (n-y)*log(1-p)) -
         N/2*log(s2b) - 1/2/s2b*sum(b^2) - 1/2*sum(log(wt+1/s2b))
  LSIG<- c(LSIG,lsig)      # exp-family likelihood
}


# ........................  plot of log-likelihood for sig
LSIG<- LSIG-max(LSIG)
  plot(SIG,LSIG,xlab=expression(sigma[b]),
       ylab='Log-likelihood',type='n')
  lines(SIG,LSIG,lty='dotted',lwd=1.52)
  title(expression('Comparing likelihoods'))
# lines(SIG,LSIG2,lty=2)
  abline(v=sqrt(.0541),h=log(.15),lwd=.3)


# ..................... exact profile likelihood from ex17-6a.r
ESIG <- seq(.001,.6,len=20)
EPROF<-  c(-1.171406909, -1.106658507, -0.936678576, -0.702569256,
          -0.454335465, -0.236323992, -0.079413698,  0.000000000,
          -0.002852236, -0.085023753, -0.239262074, -0.456445355,
          -0.727120174, -1.042381450, -1.394319476 ,-1.776186584,
          -2.182371351, -2.608234921, -3.049836886, -3.503447502)
lines(ESIG,EPROF)

