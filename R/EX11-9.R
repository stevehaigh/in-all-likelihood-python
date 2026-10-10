# Example 11.9

x <- scan('software.dat')
  tt<- 25.4
  x<- x/3600
  n<- length(x)

par(mfrow=c(2,2))
hist(x,nclass=15,xlab='CPU time (hours)',ylab='Number',main='')
  points(x,rep(0,length(x)),pch='|',cex=.4,lwd=.2)
  title(expression('(a) Software failures'))

sumx<- sum(x)
fun1 <- function(alpha,beta){
        -alpha/beta*(exp(beta*tt)-1) + n*log(alpha)+ beta*sumx
        }

aa<- seq(2*n,5*n,len=20)
  bb <- seq(-4.5,-1.5,len=20)
  aa<- aa/tt; bb<- bb/tt

loglik<- outer(aa,bb,fun1)
  maxlik <- max(loglik)
  lik<- exp(loglik - maxlik)
contour(aa,bb,lik,lev=seq(.1,1,by=.2),
        xlab=expression(alpha),
        ylab=expression(beta))
title(expression('(b) Likelihood contours'))


# minimization
fn <- function(p){
   alpha<- p[1]
   beta<- p[2]
   alpha/beta*(exp(beta*tt)-1) - n*log(alpha)- beta*sumx
   }

xmin<- nlm(fn,c(20,-.14))
  a<- xmin$est[1]
  b<- xmin$est[2]

hist(x,xlab='CPU time (hours)',ylab='Number',nclass=15,main='')
  xx<- seq(0,tt,len=40)
  xy<- a* exp(b*xx)
  xy <- xy/sum(xy)/(xx[2]-xx[1])*n*2
  lines(xx,xy)
  title(expression('(c) Intensity estimate'))


# checking the Poisson assumption:
# using exponential trend result:
y<- x[-1] - x[-n]
lam<- a*exp(b*x)
yy<- lam[-n]*y
plot(yy,xlab='Failure number',ylab='Interval',type='l')
  title(expression('(d) Interval between failures'))




