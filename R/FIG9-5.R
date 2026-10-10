# Figure 9.5: 

n<- 10
mu<-1
th<- seq(.2,4,len=100)

nrep<- 500
est<- NULL
set.seed(5)
for (i in 1:nrep){
  x<- -mu*log(runif(n))
  ll<- -n*th*log(mu) -n*log(gamma(th)) + (th-1)* sum(log(x))
  ll<- exp(ll-max(ll))
  ll<- ll/sum(ll)/(th[2]-th[1])

  ll<- n*th*log(th/mu) +(th-1)* sum(log(x))  - 
    th*sum(x)/mu - n*log(gamma(th))
  ll<- exp(ll-max(ll))
  est<- c(est,min(th[ll==max(ll)]))
}


#.................................. saddlepoint approx 
dgamma<- diff(log(gamma(th)),l=2)/2/(th[2]-th[1])
  d2gamma<- diff(log(gamma(th)),d=2)/(th[2]-th[1])^2
  ths<- th[2:99]
lg<- n*log(gamma(ths)) + 0.5*log(d2gamma-1/ths) -
    n*((ths-1)*dgamma + 1*log(ths) - ths)
  gtheta<- exp(lg-max(lg))
  gtheta<- gtheta/sum(gtheta)/(th[2]-th[1])  # normalizing


par(mfrow=c(2,2))
a<- qqnorm(est,xlab='Normal quantiles',
     ylab=expression(hat(beta)),plot=F)
  plot(a,xlab='Normal quantiles',
      ylab=expression(hat(beta)),type='n')
  points(a,cex=.4,lwd=.3)
  title(expression('(a) Normal plot'))

hist(est,prob=T,nclass=40,main='',
     xlab=expression(hat(beta)),ylab='Density',
     ylim=c(0,1.4))
  lines(ths,gtheta,lwd=.3)  # magic formula
  title(expression('(b)'))

