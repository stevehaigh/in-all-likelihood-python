
par(mfrow=c(2,2))

x <- c(0.88,1.07,1.27,1.54,1.91,2.27,3.84,4.5,4.64,9.41)
n<- length(x)

fun0<- function(mu,sigma){
      a<- -n/2*log(sigma^2) - 
          (sum(x^2) - 2*mu*sum(x) + n*mu^2)/(2*sigma^2)
      -a
      }

ll<-NULL
np<-20
mu<-  seq(mean(x)-3,mean(x)+3,len=np)
sx<- sqrt(var(x))
sigma<- seq(sx/2.5,sx*2,len=np)


ll2<- outer(mu,sigma,'fun0')
like2<- exp(min(ll2)-ll2)
  contour(mu,sigma^2,like2,xlab=expression(mu),
          ylab=expression(sigma^2),
          level=c(.1,.3,.5,.7,.9))
  title(expression('(a) Likelihood contour'))


# for (i in 1:np){
#   li<- ms(~fun0(mu[i],phi),start=list(phi=2))$value
#   ll<- c(ll,li)
# }
# like<- exp(min(ll)-ll)

# profile likelihood
# like<- apply(like2,1,max)
np<-100
mu<- seq(mean(x)-3,mean(x)+3,len=np)
for (i in 1:np){
  shat2<- sum((x-mu[i])^2)/n
  lli <- -n/2 *log(shat2)
  ll<- c(ll,lli)
}

like<- exp(ll-max(ll))
plot(mu,like,xlab=expression(mu),ylab='Likelihood',type='n')
  lines(mu,like,lwd=.3)
  abline(h=.15)
title(expression(paste('(b) Likelihood of ',mu)))

ll<- fun0(mu,sigma=1)
  like<- exp(min(ll)-ll)
  lines(mu,like,lty='dotted',lwd=1.52)

se <- sqrt(var(x)*(n-1)/n)/sqrt(n)
  xbar<- mean(x)
elike <- dnorm(xbar,mean=mu,sd=se)
  elike<- elike/max(elike)
lines(mu,elike,lty=2)


