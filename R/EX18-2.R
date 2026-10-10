# Example 18.2

x<- scan('air.dat',skip=9)
  x<- matrix(x,byrow=T,ncol=7)
  y<- x[,1]
  x<- x[,3]
  ord<- order(x)
  x<- x[ord]
  y<- y[ord]
n<- length(x)

a<- lm(log(y)~log(x))  # parametric fit
ly<- log(y)
  lx<- log(x)

source('msmooth.r')   # contains the smoothing function
par(mfrow=c(2,2))

### ...d=1
for (lambda in c(5,.5))
{
  plot(x,y,xlab='Number of factories',
         ylab='Sulphur dioxide',type='n',log='xy')
  points(x,y,cex=.6)
  lines(x,exp(a$fit),lty=2)

  smo<- msmooth(lx,ly,npar=21,lambda=lambda,d=1) 
  lines(exp(smo$x),exp(smo$y))
  if (lambda==5) title(expression(paste('d=',1,', ',lambda,'= 5')))
  if (lambda==0.5) title(expression(paste('d=',1,', ',lambda,'= 0.5')))
}

###...d=2
for (lambda in c(5,.5))
{
  plot(x,y,xlab='Number of factories',
         ylab='Sulphur dioxide',type='n',log='xy')
  points(x,y,cex=.6)
  lines(x,exp(a$fit),lty=2)

  smo<- msmooth(lx,ly,npar=21,lambda=lambda,d=2)   
  lines(exp(smo$x),exp(smo$y))
  if (lambda==5) title(expression(paste('d=',2,', ',lambda,'= 5')))
  if (lambda==0.5) title(expression(paste('d=',2,', ',lambda,'= 0.5')))
}

