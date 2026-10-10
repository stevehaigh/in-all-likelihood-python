
# Example 5.13

x<-3
th<- seq(0,10,len=100)
ll<- -th + x*log(th)
  like<- exp(ll-max(ll))

par(mfrow=c(1,1))
plot(th,like,xlab=expression(theta),
     ylab='Likelihood',type='n')
  lines(th,like,lwd=.5)
  abline(h=0.15)
  abline(v=c(.62,8.76),lwd=.3)
  title(expression('Exact interval and the likelihood'))
