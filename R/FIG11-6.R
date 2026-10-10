# Figure 11.6

x <- scan('software.dat')
  tt<- 25.4
  x<- x/3600
  n<- length(x)

par(mfrow=c(2,2))

# minimization
fn <- function(p){
   alpha<- p[1]
   beta<- p[2]
   alpha/beta*(exp(beta*tt)-1) - n*log(alpha)- beta*sumx
   }
xmin<- nlm(fn,c(20,-.14))
  a<- xmin$est[1]
  b<- xmin$est[2]

# checking the Poisson assumption:
require(ts)

y<- diff(x)
lam<- a*exp(b*x)
yy<- lam[-n]*y
acf(yy, main='')
  title('ACF between failures',cex=.6)

aa<- cumsum(1/c(length(yy):1))
  qqplot(aa,yy,xlab='Ordered exponentials',ylab='Ordered data',cex=.6)
  abline(0,1,lty=2,lwd=.6)
  title('Checking exponential intervals',cex=.6)




