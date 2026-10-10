# compute the smooth estimate of f(x) in y=f(x) + error
#        lambda is the smoothing parameter
#        npar   is the number of equispaced points of x
#        d      is degree of differencing
# output is a list of  x, fhat(x) at gridded locations of x


msmooth<- function(x,y,npar=21,lambda=10,d=2)
{
  xx<- seq(min(x)-.0001,max(x),len=npar)
  cutx<- cut(x,breaks=xx)
  xx1<- xx[-1] - (xx[2]-xx[1])/2
  midx<- exp(xx1[cutx])

# bin statistics
w <- as.numeric(table(cutx))
ybar<- as.numeric(tapply(y,cutx,mean))
  ybar[is.na(ybar)]<- 0.

delta<- matrix(0,ncol=(npar-1),nrow=(npar-2))
for (i in 1:(npar-2)){
  delta[i,i]<- -1
  delta[i,i+1]<- 1
 }
if (d==1) {R1 <- t(delta) %*% delta}

if (d==2) {
  delta2<- delta[-1,-1]%*% delta
  R1 <- t(delta2) %*% delta2
  }

fhat<- solve((diag(w) + lambda*R1),w*ybar)
return(list(x=xx1,y=fhat))
}

