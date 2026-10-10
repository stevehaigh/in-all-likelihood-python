# Example 18.3

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

#........ use rough partition to get a starting value for sigma^2
npar<- 11
  xx<- seq(min(lx)-.0001,max(lx),len=npar)
  cutx<- cut(lx,breaks=xx)
# bin statistics
cat(table(cutx),'\n')
  w <- as.numeric(table(cutx))
  s2<- as.numeric(tapply(log(y),cutx,var))
  s2[is.na(s2)]<- 0.
  s2 <- sum(w*s2)/sum(w)  # starting value of sigma^2

# finer partition:
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

par(mfrow=c(2,2))
for (d in c(1,2)){  # ------------------------------ differencing loop

# starting value
  s2<- .35; lambda<-5  
  s2b <- s2/lambda

if (d ==1) R1 <- t(delta) %*% delta
if (d ==2){   # degree of differencing d=2
  delta2<- delta[-1,-1]%*% delta
  R1 <- t(delta2) %*% delta2
}

olds2<- 100
olds2b<- 100
i<-0
while ((abs(olds2-s2)/s2 + abs(olds2b-s2b)/s2b) >.0001)
{
  i<- i+1
  olds2<- s2
  olds2b<- s2b
  fhat<- solve((diag(w) + lambda*R1),w*ybar)
  ffhat<- rep(fhat,w)
  err <- (ly-ffhat)
  smat <- solve(diag(w) + lambda*R1)
  df <- sum(diag(smat)*w)
  s2 <- sum(err^2)/(length(ly)- df)   # update s2

  brb <- c(fhat%*%R1 %*%fhat) 
  mat <- smat * R1
  s2b <- 1/(npar-1-d)*(brb + s2*sum(mat)) # npar-1 = number of bins
                                       # npar-3 = number of iid differences
                                       #          for order d=2
                                       #    this divisor is very important!!
  lambda<- s2/s2b
  cat('Iter= ',i,'s2= ',s2,'s2b= ',s2b,'lambda= ',lambda,'df= ',df,'\n')
}

# plot the final choice
  plot(x,y,xlab='Number of factories',
           ylab='Sulphur dioxide',type='n',log='xy')
  points(x,y,cex=.6)
  a<- lm(log(y)~log(x))
  lines(x,exp(a$fit),lty=2)
  a<- lm(ly~poly(lx,2))     # quadratic fit
  lines(x,exp(a$fit),lty=2)

  fhat<- solve((diag(w) + lambda*R1),w*ybar)
  lines(exp(xx1),exp(fhat))
  if (d==1) title(expression(paste('d= ',1,', ',hat(lambda),'= 6.2')))
  if (d==2) title(expression(paste('d= ',2,', ',hat(lambda),'= 99.2')))
} # ------------------------------------------- end differencing loop

