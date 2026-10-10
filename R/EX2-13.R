# li() is a function to compute likelihood interval
#   using some interpolation
source('li.r')

par(mfrow=c(2,2))
th<- seq(0.01,.99,len=100)
x<- 8
n<- 10
like <- dbinom(x,n,th)
like<-  like/max(like)

plot(th,like,xlab=expression(theta),
     ylab='Likelihood',type='l')
abline(h=c(.04,.15),lwd=.3)
for (a in c(0.04,.15)){
  cat('cutoff=',a, 'Interval=',li(th,like,a),'\n')
}

title(expression('Likelihood intervals'))
