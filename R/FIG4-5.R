
# Section 4.6, Figure 4.5

x<- 170
y<- 190

theta<- seq(.7,1.7,len=100)
ll<- y*log(theta) - (x+y)*log(1+theta)
  like<- exp(ll-max(ll))

par(mfrow=c(2,2))
plot(theta,like,xlab=expression(theta),
     ylab='Likelihood',type='n')
  lines(theta,like)
mle<- y/x
  abline(v=mle,lwd=.3)
  abline(v=1,lwd=.3)
  abline(h=.15,lwd=.3)
  title(expression('(a) Likelihood of rate ratio'),cex=.7)

source('li.r')
cat('mle= ',mle,',  95% CI=',li(theta,like,0.15),'\n')


xx<- c(20,40,75,10)
yy<- c(35,54,65,2)
theta<- seq(.01,4,len=100)
plot(theta,like, xlab=expression(theta),
    ylab='Likelihood',type='n')
  abline(h=.15,lwd=.3)

for (i in 1:4){
 x<- xx[i];  y<- yy[i]
 ll<- y*log(theta) - (x+y)*log(1+theta)
 like<- exp(ll-max(ll))
 lines(theta,like,lwd=.2)
 mle<- y/x
 cat('mle= ',mle,',  95% CI=',li(theta,like,0.15),'\n')

}

title(expression('(b) For four groups'))

