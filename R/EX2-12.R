
par(mfrow=c(2,2))
x<-8; n<-10
theta<- seq(0.5,.99,len=100)

like <- dbinom(8,10,theta)
  like1<- like/max(like)
  ll<- log(like1)

that<- x/n
  ihat<- n/that/(1-that)
  la <- -ihat/2*(theta-that)^2
  ra<- range(ll)

plot(theta,ll,type='n',xlab=expression(theta),
     ylab='log-likelihood')
  lines(theta,ll,lwd=.4)
  lines(theta,la,lty='dotted',lwd=1.52)

title(expression('(a) n=10, x=8'))

sco<- -1/sqrt(ihat)*diff(ll)/diff(theta)
the<- sqrt(ihat)*(theta[-1]-that)

plot(the,sco,type='n',xlab=expression(paste('Scaled ',theta)),
     xlim=c(-2.5,2.5),
     ylab='Scaled score function')
  lines(the,sco)
  abline(0,1,lty='dotted',lwd=1.52)
  title(expression('(b) Linearity of score function'))


x<-80; n<-100
theta<- seq(0.5,.99,len=100)

ll<- x*log(theta) + (n-x)*log(1-theta)
  ll<- ll-max(ll)

that<- x/n
  ihat<- n/that/(1-that)
  la <- -ihat/2*(theta-that)^2
plot(theta,ll,ylim=ra,type='n',xlab=expression(theta),
     ylab='log-likelihood')
  lines(theta,ll,lwd=.4)
  lines(theta,la,lty='dotted',lwd=1.52)
  title(expression('(c) n=100, x=80'))


sco<- -1/sqrt(ihat)*diff(ll)/diff(theta)
the<- sqrt(ihat)*(theta[-1]-that)
plot(the,sco,type='n',xlab=expression(paste('Scaled ',theta)),
     xlim=c(-2.5,2.5),
     ylab='Scaled score function',ylim=c(-2.5,2.5))
  lines(the,sco)
  abline(0,1,lty='dotted',lwd=1.52)
  title(expression('(d) Linearity of score function'))
                       
