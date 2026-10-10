# Example 4.9

x<- c(23.9,27.3,  0.2,  3.6 , 6.0,  0.9,  0.9,
    0.5 , 0.2 ,14.2,  6.2,  0.05 , 3.9,  0.2,  0.1)
n<-length(x)

fun0<- function(mu,phi){
      alpha<-1/phi
      lam<- 1/(phi*mu)
      a<- -n*lgamma(alpha)+ n*alpha*log(lam) + 
         (alpha-1)*sum(log(x))- lam*sum(x)
      -a
      }

ll<-NULL
np<-20
mu<-  seq(mean(x)/3,mean(x)*3,len=np)
phi<- seq(1,4,len=np)
alp<- 1/phi
lam<- 1/(phi*mu)

ll2<- outer(mu,phi,'fun0')
  like2<- exp(min(ll2)-ll2)

par(mfrow=c(2,2))
contour(mu,phi,like2,
        xlab=expression(mu),ylab=expression(phi),
        level=c(.1,.3,.5,.7,.9))
title(expression('(a) Likelihood contour'))


# profile likelihood
like<- apply(like2,1,max)
plot(mu,like,xlab=expression(mu),
     ylab='Likelihood',type='n')
  lines(mu,like,lwd=.3)
  abline(h=.15)
  title(expression(paste('(b) Likelihood of ',mu)))

np<-100
mu<- seq(mean(x)/3,mean(x)*3,len=np)
ll<- fun0(phi=2.35,mu)
  like<- exp(min(ll)-ll)
  lines(mu,like,lty='dotted',lwd=1.5)

mu<- seq(mean(x)/3,mean(x)*3,len=np)
ll<- fun0(phi=1,mu)
  like<- exp(min(ll)-ll)
  lines(mu,like,lty='dashed')

