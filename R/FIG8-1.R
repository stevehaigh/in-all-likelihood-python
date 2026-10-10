# Figure 8.1     
# variations of score functions:

set.seed(7)
n<- 10
#............................... normal:
  t0<- 4
  x<- rnorm(n,t0)
  theta<- seq(t0/2,t0*2,len=40)
  stheta<- n*(mean(x)-theta)

par(mfrow=c(2,2))
plot(theta,stheta,type='n',
     xlab=expression(theta),ylab='Score',cex=.6)
  lines(theta,stheta,lwd=.4)
  title(expression('(a) Normal n=10'))
  text(6.5,5.5,expression(paste('true ',theta,'=4')))
  abline(v=t0,h=0)


for(i in 1:20){
  x<- rnorm(n,t0)
  stheta<- n*(mean(x)-theta)
  lines(theta,stheta,lwd=.1)
}


# ............................... Poisson:
t0<- 4
x<- rpois(n,t0)
theta<- seq(t0/2,t0*2,len=40)
stheta<- -n + sum(x)/theta

plot(theta,stheta,type='n',xlab=expression(theta),
     ylab='Score',ylim=c(-5,15),cex=.6)
for(i in 1:20){
  x<- rpois(n,t0)
  stheta<- -n + sum(x)/theta
  lines(theta,stheta,lwd=.1)
}
abline(v=t0,h=0)
title(expression('(b) Poisson n=10'))


# ............................... Binomial
t0<- .4
x<- rbinom(1,n,t0)
theta<- seq(.1,.8,len=40)
stheta<- x/theta - (n-x)/(1-theta)

plot(theta,stheta,type='n',xlab=expression(theta),
     ylab='Score',ylim=c(-25,35),cex=.6)
for(i in 1:20){
  x<- rbinom(1,n,t0)
  stheta<- x/theta - (n-x)/(1-theta)
  lines(theta,stheta,lwd=.1)
}
abline(v=t0,h=0)
title(expression('(c) Bernoulli n=10'))


# ............................... Cauchy
scauchy<-function(x,mu)  # score function
{
  theta<-mu+ seq(-20,20,len=100)
  score<-rep(0,100)
  for (i in 1:100)
      { score[i]<-sum(dcauchy(x,location=theta[i])*
                     pi*2*(x-theta[i]))
      }
  list(theta=theta,score=score)
}  

n<-10
t0<-4
x<-rcauchy(n)+t0
sx<-scauchy(x,t0)

plot(sx$theta,sx$scor,ylim=c(-7.5,7.5),
     type="n",cex=.6,xlab=expression(theta),
     ylab='Score')
abline(h=0)
lines(sx$theta,sx$scor,lwd=.4)
for (i in 1:20){
  x<-rcauchy(n)+t0
  sx<-scauchy(x,t0)
  lines(sx$theta,sx$scor,lwd=.1)
}
abline(v=t0,h=0)
title(expression("(d) Cauchy n=10"))
