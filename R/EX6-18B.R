# Example 6.19b: to produce Figure 6.9  and the second table
# in page 180

x<- scan('air.dat',skip=9)
   x<- matrix(x,byrow=T,ncol=7)
y<- x[,1]
x<- x[,3]
  ord<- order(x)
  x<- x[ord]
  y<- y[ord]
  n<- length(x)

lx<- log(x)
xreg<- lm(y~lx+lx^2)
  asum<- summary(xreg)
  a1<- qqnorm(xreg$res,plot=F)

# normal plots:
par(mfrow=c(2,2))
plot(a1,xlab='Standard normal quantiles',ylab='Residuals',
     type='n')
  points(a1,cex=.6)
  qqline(xreg$res,lty=2,lwd=.4)
  title(expression(paste('(a) ',lambda,'=1')))

# finding lambda-hat
ll<-NULL
nl<-20
lambda<-seq(.01,1.5,len=nl)
for (i in 1:nl){
  yl<- (y^lambda[i] - 1)/lambda[i]
  xreg<- lm(yl~poly(lx,2))
  sl2<- sum(xreg$res^2)/(n-3)
  lli <- -n/2*log(sl2) + (lambda[i]-1)*sum(log(y))
  ll<- c(ll,lli)
}
ll<- ll-max(ll)
plot(lambda,ll,xlab=expression(lambda),ylab='Log-likelihood',
     type='n')
  lines(lambda,ll)
  title(expression(paste('(b) Profile likelihood of ',lambda)))
  lmax<- max(lambda[ll==max(ll)])
  #abline(v=lmax)

plot(x,y,xlab='Industries',log='xy',
         ylab='Sulphur dioxide',type='n')
  points(x,y,cex=.6)
  title(expression('(c) Transform both axes'))

lx2<- lx^2
xreg<- lm(log(y)~ lx+lx2)
  bsum<- summary(xreg)
  print(bsum)
  lines(x,exp(xreg$fit),lty=2)

a1<- qqnorm(xreg$res,plot=F)
plot(a1,xlab='Standard normal quantiles',ylab='Residuals',
     type='n')
  points(a1,cex=.6)
  qqline(xreg$res,lty=2,lwd=.4)
  title(expression(paste('(d) ',lambda,'=0')))

