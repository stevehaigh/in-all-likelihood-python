# Figure 9.2: variations of score statistics:


set.seed(13)
n<- 10
  t0<- 4
  x<- rpois(n,t0)

theta<- seq(t0/2,t0*2,len=40)
stheta<- -n + sum(x)/theta

par(mfrow=c(2,2))
plot(theta,stheta,type='n',
    xlab=expression(theta),
    ylab='Score statistic')
  lines(theta,stheta,lwd=.6)
  title(expression('(a) Score function'))
  text(6.5,4.,expression(paste('true ',theta,'=4')))
  text(6.5,6.,expression(paste('n=10, ',sum(x),'=36')))
  text(6.5,8,expression('Poisson data'))
  abline(v=t0,h=0)


plot(theta,stheta,type='n',
   xlab=expression(theta),
   ylab='Score statistic',cex=.6)
for(i in 1:25){
  x<- rpois(n,t0)
  stheta<- -n + sum(x)/theta
  lines(theta,stheta,lwd=.1)
}
abline(v=t0,h=0)
title(expression('(b) Repeated sampling'))

x <- matrix(rpois(100*n,t0),ncol=n)
  sumx<- x %*% rep(1,n)
  sumx<- c(sumx)
  xbar <- sumx/n

s0<- -n + sumx/t0
s2<- n/xbar    #  Fisher information

hist(s0,xlab=expression(paste('S(',theta,'=4)')),
     ylab='Count', main='',nclass=10,
     xlim=c(-4,4))
 y<- seq(-5,5,len=40)
lines(y,100*dnorm(y,0,sqrt(10/t0)))
title(expression(paste('(c) Distribution of S(',theta,'=4)')))

hist(s2,nclass=20,
     xlab=expression(paste('I(',theta,'=4)')),
     ylab='Count', main='')
title(expression(paste('(d) Distribution of I(',theta,'=4)')))
cat('Mean of I(theta=4)=',mean(s2), '(theory=10/4=2.5)','\n')

