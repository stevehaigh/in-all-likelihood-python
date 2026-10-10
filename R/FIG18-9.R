# Figure 18.9

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

# Delta matrix
delta<- matrix(0,ncol=(npar-1),nrow=(npar-2))
for (i in 1:(npar-2)){
  delta[i,i]<- -1
  delta[i,i+1]<- 1
 }


par(mfrow=c(2,2))
for (d in c(1,2)){  # ------------------------------------------- differencing

if (d==1) {s2<- .3679; lambda<-6.2 } # estimated values from ex18-3.r
if (d==2) {s2<- .3775; lambda<-99.2 } # estimated values from ex18-3.r
s2b <- s2/lambda

print(c(s2,lambda))
# fine cut
npar<- 21
  xx<- seq(min(lx)-.0001,max(lx),len=npar)
  cutx<- cut(lx,breaks=xx)
  xx1<- xx[-1] - (xx[2]-xx[1])/2
  midx<- exp(xx1[cutx])

# bin statistics
w <- as.numeric(table(cutx))
ybar<- as.numeric(tapply(log(y),cutx,mean))
  ybar[is.na(ybar)]<- 0.

if (d==1) R1 <- t(delta) %*% delta
if (d==2){   # degree of differencing d=2
  delta2<- delta[-1,-1]%*% delta
  R1 <- t(delta2) %*% delta2
}

# plot the final choice
plot(x,y,xlab='Number of factories',
           ylab='Sulphur dioxide',type='n',log='xy')
  points(x,y,cex=.6)
 
  fhat<- solve((diag(w) + lambda*R1),w*ybar)
    lines(exp(xx1),exp(fhat))

  smat <- solve(diag(w) + lambda*R1)  # inverse of Fisher information
  upper<- fhat+ 1.96* sqrt(s2)* sqrt(diag(smat))
    lines(exp(xx1),exp(upper),lwd=.3)
  lower<- fhat- 1.96* sqrt(s2)* sqrt(diag(smat))
    lines(exp(xx1),exp(lower),lwd=.3)
  
  if (d==1) title(expression(paste('d= ',1,', ',hat(lambda),'= 6.2')))
  if (d==2) title(expression(paste('d= ',2,', ',hat(lambda),'= 99.2')))

} # -------------------------------------------------------- end differencing loop



