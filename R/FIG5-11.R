# Figure 5.11


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
result[1,2]<- 3.68  # from confidence interval

tt<- seq(0.001,7,len=100)
conf<-NULL
for (theta in tt){
  inside<- ifelse((theta< result[,2] & theta>result[,1]),
                  1,0)
  ai<- sum(dpois(xx[inside==1],theta))
  conf<- c(conf,ai)
}

par(mfrow=c(1,1))
plot(tt,conf,type='l',ylim=c(.8,1),
     xlab=expression(theta),
     ylab='Coverage probability')
abline(h=.95,lty=2)
title(expression('Changing the interval at x=0'))

