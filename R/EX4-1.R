
# Example 4.1

x<-8; n<-10

theta<- seq(0.01,.99,len=100)
like <- dbinom(8,10,theta)
like1<- like/max(like)
print(li(theta,like1,exp(-1.92)))

par(mfrow=c(1,1))
plot(theta,like1,type='n',xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like1,lwd=.3)
  abline(h=.15,lwd=.3)
  text(.2,.2,'95% confidence',cex=.8)
  title(expression('Likelihood from Bernoulli trial'))

                         
