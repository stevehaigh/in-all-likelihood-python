

par(mfrow=c(1,1))
      
# censored data:
theta<- seq(.0,.3,len=100)
like <- pbinom(10,100,theta)
  like1<- like/max(like)

a <- theta[like1>.15]
print(range(a))

like<- dbinom(5,100,theta)
  like2<- like/max(like)
b <- theta[like2>.15]
  print(range(b))

plot(c(0,.3),range(c(like1,like2)),type='n',
     xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like1)
  lines(theta,like2,lty=2)
  text(.15,.4,'x<11',cex=.8)
  text(.055,.4,'x=5',cex=.8)
  abline(h=.15,lwd=.3)

title(expression('Binomial likelihood, n=100'))



dev.off()

