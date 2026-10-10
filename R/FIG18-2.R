# Figure 18.2

x<- scan('air.dat',skip=9)
  x<- matrix(x,byrow=T,ncol=7)
  y<- x[,1]
  x<- x[,3]
  ord<- order(x)
  x<- x[ord]
  y<- y[ord]
  n<- length(x)


a<- lm(log(y)~log(x))
ly<- log(y)
lx<- log(x)

par(mfrow=c(2,2))
for (npar in c(5,11,21,42))
{
  plot(x,y,xlab='Number of factories',
         ylab='Sulphur dioxide',type='n',log='xy')
  points(x,y,cex=.6)
  lines(x,exp(a$fit),lty=2)

  xx<- seq(min(lx)-.0001,max(lx),len=npar)
  abline(v=exp(xx),lwd=.01)
  cutx<- cut(lx,breaks=xx)
  meanly<- tapply(ly,cutx,mean)
  xx1<- xx[-1] - (xx[2]-xx[1])/2
  lines(exp(xx1[is.na(meanly)==F]),exp(meanly[is.na(meanly)==F]))

  title(paste('Number of bins = ',npar-1),cex=.6)
}


