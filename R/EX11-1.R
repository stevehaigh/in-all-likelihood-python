# Example 11.1

par(mfrow=c(2,2))
require(ts)
source('arsim.r')

set.seed(7)
n<-100
x<- ar.sim(n,0.5)   # simulate AR(1) data
  xt<- x[-1]
  xt1<- x[-n]

plot(x,xlab='time',ylab=expression(x[t]),type='l')
title(expression(paste('(a) |',phi,'| is far from one')))

phihat<- sum(xt*xt1)/sum(xt1^2)
  se<- 1/sqrt(sum(xt1^2))
  phi<-seq(phihat-3*se,min(phihat+3*se,.99),len=100)

ll2<- -1/2*(sum(xt^2) + phi^2*sum(xt1^2) - 2*phi*sum(xt*xt1))
  ll2<- ll2-max(ll2)
  l2<- exp(ll2)
plot(phi,l2,xlab=expression(phi),ylab='Likelihood',type='n')
  lines(phi,l2)

ll1<- 1/2*log(1-phi^2) - 1/2*(1-phi^2)*x[1]^2
  ll<- ll1+ll2
  ll<- ll-max(ll)
  lik<- exp(ll)
  lines(phi,lik,lty='dotted',lwd=1.52)
  title(expression(paste('(b) Likelihood of ',phi)))


# ...... second example
set.seed(7)
n<-100
  x<- ar.sim(n,0.95)     # simulated AR(1) data
  plot(x,xlab='time',ylab=expression(x[t]),type='l')
  title(expression(paste('(c) ',phi,' is near one')))

xt<- x[-1]
xt1<- x[-n]

phihat<- sum(xt*xt1)/sum(xt1^2)
se<- 1/sqrt(sum(xt1^2))
phi<-seq(phihat-3*se,min(phihat+3*se,.99),len=100)

ll2<- -1/2*(sum(xt^2) + phi^2*sum(xt1^2) - 2*phi*sum(xt*xt1))
ll2<- ll2-max(ll2)
l2<- exp(ll2)
plot(phi,l2,xlab=expression(phi),ylab='Likelihood',type='n')
lines(phi,l2)

ll1<- 1/2*log(1-phi^2) - 1/2*(1-phi^2)*x[1]^2
  ll<- ll1+ll2
  ll<- ll-max(ll)
  lik<- exp(ll)
lines(phi,lik,lty='dotted',lwd=1.52)
title(expression(paste('(d) Likelihood of ',phi)))


