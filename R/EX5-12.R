# Example 5.12
# confidence intervals for Poisson models:

xx<- 0:15
tmax<- max(xx)+ 2*sqrt(max(xx))+2
th<- seq(.0001,tmax,len=tmax*1000)

ci<-NULL
for (x in xx){
  pright<- 1- ppois((x-1),th) - 0.0*dpois(x,th)
  lo<- min(th[pright>.025])
  pleft <- ppois(x,th) - 0.0*dpois(x,th)
  up<- max(th[pleft>.025])
  ai<- c(lo,up)
  ci<- rbind(ci,ai)
}
print(round(ci,2))

plot(xx,ci[,1],ylim=range(ci),
     xlab='Observed x',ylab=expression(theta),type='n')
for (x in xx) lines(c(x,x),ci[(x+1),])
lines(xx,xx,lty=2)
title(expression('(a) Interval limits'))


tt<- seq(0.001,7,len=100)
confci<-NULL
for (theta in tt){
  inside<- ifelse((theta<= ci[,2] & theta>=ci[,1]),
                  1,0)
  ai<- sum(dpois(xx[inside==1],theta))
  confci<- c(confci,ai)
}

plot(tt,confci,type='l',ylim=c(.8,1),
     xlab=expression(theta),ylab='Coverage probability')
abline(h=.95,lty=2)
title(expression('(b) Coverage probability'))
