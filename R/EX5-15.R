#  Exact intervals for Binomial
#  Example 5.15

n<-10
xx<- 0:n
th<- seq(.0001,.9999,len=100)

ci<-NULL
for (x in xx){
  pright<- 1- pbinom((x-1),n,th) - 0.0*dbinom(x,n,th)
  lo<- min(th[pright>.025])
  pleft <- pbinom(x,n,th) - 0.0*dbinom(x,n,th)
  up<- max(th[pleft>.025])
  ai<- c(lo,up)
  ci<- rbind(ci,ai)
}

par(mfrow=c(2,2))
plot(xx,ci[,1],ylim=range(ci),
     xlab='Observed x',
     ylab=expression(theta),type='n')
  for (x in xx) lines(c(x,x),ci[(x+1),])
  lines(xx,xx/n,lty=2)
  title(expression('(a) Exact 95% confidence intervals'))


tt<- seq(0.001,.999,len=100)
confci<-NULL
for (theta in tt){
  inside<- ifelse((theta<= ci[,2] & theta>=ci[,1]),
                  1,0)
  ai<- sum(dbinom(xx[inside==1],n,theta))
  confci<- c(confci,ai)
}

plot(tt,confci,type='l',ylim=c(.8,1),
     xlab=expression(theta),ylab='Coverage probability')
abline(h=.95,lty=2)
title(expression('(b) Coverage probability'))
