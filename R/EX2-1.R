
# Examples 2.1 
par(mfrow=c(2,2))
                         
# .......................................Ex 2-1: (a) censored data:
theta<- seq(.01,.3,len=100)

like <- pbinom(10,100,theta)
  like1<- like/max(like)

like<- dbinom(5,100,theta)
  like2<- like/max(like)

plot(c(0,.3),range(c(like1,like2)),type='n',
     xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like1)
  lines(theta,like2,lty='dotted',lwd=1.5)
  text(.15,.4,'x<11',cex=.8)
  text(.055,.4,'x=5',cex=.8)
  title(expression('(a) Germinating probability'))


