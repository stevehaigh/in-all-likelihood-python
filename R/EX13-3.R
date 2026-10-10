# Example 13.3

# distance to gamma:
set.seed(13)

bt<- 4
  x<- rgamma(10000,bt)
  meang<- mean(log(dgamma(x,bt)))
  mu<- 0:12
  sig<-sqrt(bt)

kl<-NULL
for (i in 1:length(mu)){
  meanf<- mean(log(dnorm(x,mu[i],sig)))
  kl<- c(kl,meang- meanf)
 }

plot(mu,kl,type='n',xlab=expression(mu),
  ylab='KL distance')
  lines(mu,kl)
  print(min(kl))
  title(expression(paste('(a) Gamma(4,1) to N(',mu,',',sigma^2,')')))

sig<- 1
kl<-NULL
for (i in 1:length(mu)){
  meanf<- mean(log(dnorm(x,mu[i],sig)))
  kl<- c(kl,meang- meanf)
 }
lines(mu,kl,lty='dotted',lwd=1.52)
  abline(h=0,lwd=.3)
  text(7,7,'1',cex=.7)
  text(10,6,'2',cex=.7)

x<- seq(0,12,len=100)
plot(x,dgamma(x,bt),xlab='x',ylab='Density',type='n')
  lines(x,dgamma(x,bt),lwd=.4)
  lines(x,dnorm(x,bt,sqrt(bt)),lty='dotted',lwd=1.52)
  title(expression('(b) True and closest density'))


