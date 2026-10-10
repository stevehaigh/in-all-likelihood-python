
par(mfrow=c(1,1))
x<-2; n<-10

theta<- seq(0.0,1,len=100)
  like <- dbinom(x,10,theta)
  like<- like/max(like)

plot(theta,like,type='n',xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like,lwd=.3)
  title(expression('Likelihood functions'))

xx<- c(0,5,10)
for (i in 1:3){
  x<- xx[i]
  like <- dbinom(x,10,theta)
  like<- like/max(like)
  lines(theta,like,lwd=.3)
}


                         
