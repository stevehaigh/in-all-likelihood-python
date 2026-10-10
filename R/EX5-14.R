# Exact likelihood intervals for Poisson:
# Example 5.14

source('li.r')
xx<- 0:15
tmax<- max(xx)+ 2*sqrt(max(xx))+2
th<- seq(.0001,tmax,len=tmax*100)
result<-NULL
for (x in xx){
   like<- dpois(x,th)
   like<- like/max(like)
   a<- li(th,like,exp(-1.96^2/2))
   result<- rbind(result,a)
}
ci<-result

par(mfrow=c(2,2))
plot(xx,ci[,1],ylim=range(ci),
     xlab='Observed x',
     ylab=expression(theta),type='n')
for (x in xx) lines(c(x,x),ci[(x+1),])
lines(xx,xx,lty='dotted',lwd=1.5)
title(expression('(a) Interval limits'))

ci<- c(0,3)  # confidence intervals
for (x in xx[-1]){
  pright<- 1- ppois((x-1),th) - 0.0*dpois(x,th)
  lo<- min(th[pright>.025])
  pleft <- ppois(x,th) - 0.0*dpois(x,th)
  up<- max(th[pleft>.025])
  ai<- c(lo,up)
  ci<- rbind(ci,ai)
}
lines(xx,ci[,1],lty='dotted',lwd=1.5)
lines(xx,ci[,2],lty='dotted',lwd=1.5)



tt<- seq(0.001,7,len=100)
conf<-NULL
for (theta in tt){
  inside<- ifelse((theta< result[,2] & theta>result[,1]),
                  1,0)
  ai<- sum(dpois(xx[inside==1],theta))
  conf<- c(conf,ai)
}

plot(tt,conf,type='l',ylim=c(.8,1),
     xlab=expression(theta),
     ylab='Coverage probability')
abline(h=.95,lty=2)
title(expression('(b) Coverage probability'))

