
# Example 4.14

y<- c(0, 10,  1,  1,  1 , 2,  1,  4, 11 ,
     0,  5 , 2,  5 , 2,  0 , 2,  0 , 1,  3 , 0)
  n<- length(y)

phi0<- var(y)/mean(y)
cat('mean=',mean(y),',  var=',var(y),',  ratio=',phi0,'\n')

# Monte Carlo test:

a<- rpois(500*n,mean(y))   # simulated Poisson
  a<- matrix(a,ncol=n)
  amean<- c(a %*% rep(1,n))/n
  avar<- c((a-amean)^2%*% rep(1,n))/(n-1)
  phi<- avar/amean
cat('P-value=',sum(phi>phi0)/500,'\n')

par(mfrow=c(2,2))
a<- qqnorm(phi,plot=F)
plot(a,xlab='Standard normal quantiles',
     ylab=expression(hat(phi)),type='n')
  points(a,cex=.6,lwd=.2)
  title(expression(paste('(a) Simulated ',hat(phi))))

mu<- seq(.5,5,len=50)
  ll0 <- (log(mu)*sum(y) - n*mu)
  l0<- exp(ll0-max(ll0))
  ll <- (log(mu)*sum(y) - n*mu)/phi0
  l<- exp(ll-max(ll))

plot(mu,l,ylim=c(0,1),xlab=expression(mu),
    ylab='Likelihood',type='n')
  lines(mu,l,lwd=.5)
  lines(mu,l0,lty='dotted',lwd=1.5)
  title(expression(paste('(b) Likelihood of ',mu)))
  abline(v=mean(y),h=0.15,lwd=.3)


