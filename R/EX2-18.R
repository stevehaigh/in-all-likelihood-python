

x<-8; n<-10

theta<- seq(0.4,.99,len=100)
like <- dbinom(8,10,theta)
  like1<- like/max(like)
ll<- log(like1)
  a <- theta[like1>.15]
  print(range(a))

that<- x/n
  ihat<- n/that/(1-that)
  la <- -ihat/2*(theta-that)^2
  ra<- range(ll)

plot(theta,ll,type='n',xlab=expression(theta),
     ylab='Log-likelihood')
  lines(theta,ll,lwd=.4)
  lines(theta,la,lty='dotted',lwd=1.52)
  abline(h=-1.92,lwd=.4)
title(expression('(a) Probability scale'))

psi <- log(theta) - log(1-theta)
psihat <- log(that) - log(1-that)
  ihat <- x*(n-x)/n
  lpsi <- -ihat/2*(psi-psihat)^2
plot(psi,ll,type='n',xlab=expression(psi),
     ylab='Log-likelihood')
  lines(psi,ll,lwd=.4)
  lines(psi,lpsi,lty='dotted',lwd=1.52)
  abline(h=-1.92,lwd=.4)

title(expression('(b) Log-odds scale'))

                         
