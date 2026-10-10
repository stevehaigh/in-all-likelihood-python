# Figure 4.8

x<- scan('michel.dat')

par(mfrow=c(2,2))
a <- qqnorm(x,plot=F)
  plot(a$x,a$y,xlab='Standard normal quantile',
       ylab='Observed quantile',type='n')
  points(a$x,a$y,cex=.5)
  qqline(x,lty=2)
  title(expression('(a) Full data'))

a <- qqnorm(x[21:100],plot=F)
  plot(a$x,a$y,xlab='Standard normal quantile',
       ylab='Observed quantile',type='n')
  points(a$x,a$y,cex=.5)
  qqline(x[21:100],lty=2)
  title(expression('(b) Excluding first experiment'))

set.seed(7)
x <- rexp(100)
  a <- qqnorm(x,plot=F)
  plot(a$x,a$y,xlab='Standard normal quantile',
       ylab='Observed quantile',type='n')
  points(a$x,a$y,cex=.5)
  qqline(x,lty=2)
  title(expression('(c) Exponential data'))

x <- runif(100)
  a <- qqnorm(x,plot=F)
  plot(a$x,a$y,xlab='Standard normal quantile',
       ylab='Observed quantile',type='n')
  points(a$x,a$y,cex=.5)
  qqline(x,lty=2)
  title(expression('(d) Uniform data'))

