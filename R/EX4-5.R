                         
# Example 4.5: Poisson data

x<-c(3, 2, 5, 0 ,4)
  n<- length(x)

mu<- seq(.1,6,length=100)
ll<- -n*mu + sum(x)*log(mu)
like<- exp(ll-max(ll))

par(mfrow=c(1,1))
plot(mu,like,xlab=expression(theta), 
     ylab='Likelihood',type='n')
  lines(mu,like,lwd=.3)
  abline(v=mean(x),h=.15,lwd=.1)
  title('Likelihood function',cex=.7)
  text(5,.2,'95%',cex=.7)

cat('mean=',mean(x),'\n')
cat('se  =',sqrt(mean(x)/n),'\n')
source('li.r')
cat('95% CI=',li(mu,like,.15),'\n')

