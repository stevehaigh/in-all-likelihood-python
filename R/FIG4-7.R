# Figure 4.7

# ...................... heart attacks
theta<- seq(0.2,.99,len=100)
x<- 139
y<- 239

ll <- x*log(theta/(theta+1)) - y*log(theta+1)
like<- exp(ll-max(ll))

par(mfrow=c(2,2))
plot(range(theta),range(like),type='n',
     xlab=expression(theta),
     ylab='Likelihood')
  title(expression('(a) Heart attacks'))
  lines(theta,like,lwd=.4)
  abline(h=.15,lwd=.3)

# normal approx
pihat <- x/(x+y)
  sd <- sqrt(pihat/(1-pihat)^3/(x+y))
nlike <- dnorm(theta,x/y,sd)
  nlike<- nlike/max(nlike)
  lines(theta,nlike,lty='dotted',lwd=1.5)

# ...................... stroke
theta<- seq(0.7,1.99,len=1000)
x<- 119
y<- 98
llike <- x*log(theta/(theta+1)) - y*log(theta+1)
  like <- exp(llike-max(llike))

plot(range(theta),range(like),type='n',
     xlab=expression(theta),
     ylab='Likelihood')
  title(expression('(b) Strokes'),cex=.7)
  lines(theta,like,lwd=.4)
  abline(h=.15,lwd=.3)
  abline(v=1,lwd=.3)

# normal approx
pihat <-  x/(x+y)
  sd <- sqrt(pihat/(1-pihat)^3/(x+y))
nlike <- dnorm(theta,x/y,sd)
  nlike<- nlike/max(nlike)
  lines(theta,nlike,lty='dotted',lwd=1.5)

