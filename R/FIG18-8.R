# Figure 18.8

x<- scan('air.dat',skip=9)
  x<- matrix(x,byrow=T,ncol=7)
  y<- x[,1]
  x<- x[,3]
  ord<- order(x)
  x<- x[ord]
  y<- y[ord]
  n<- length(x)

ly<- log(y)
  lx<- log(x)


# partitions
npar<- 21
  xx<- seq(min(lx)-.0001,max(lx),len=npar)
  cutx<- cut(lx,breaks=xx)
  xx1<- xx[-1] - (xx[2]-xx[1])/2
  midx<- exp(xx1[cutx])
# bin statistics
w <- as.numeric(table(cutx))
ybar<- as.numeric(tapply(log(y),cutx,mean))
  ybar[is.na(ybar)]<- 0.

delta<- matrix(0,ncol=(npar-1),nrow=(npar-2))
for (i in 1:(npar-2)){
  delta[i,i]<- -1
  delta[i,i+1]<- 1
 }
delta2<- delta[-1,-1]%*% delta
R1 <- t(delta2) %*% delta2

GCV<- NULL
DF<- NULL
ll<- seq(.2,7,len=40)
for (lambda in exp(ll))
{
  fhat<- solve((diag(w) + lambda*R1),w*ybar)
  ffhat<- rep(fhat,w)
  err <- (ly-ffhat)
  smat <- solve(diag(w) + lambda*R1)
  df <- sum(diag(smat)*w)
  DF<- c(DF,df)
  s2 <- sum(err^2)/(length(ly)- df)   # update s2

  gcv<- sum(err^2)/(length(ly)- df)^2
  GCV<- c(GCV,gcv)
}

plot(exp(ll),GCV,
     xlab=expression(lambda),
     ylab='GCV',
     log='x',type='l')
