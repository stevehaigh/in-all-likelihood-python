# Example 15.5

source('meanlike.r')

f1 <- function(th,x){
     abs(x-th)
     }

xdat <- matrix(scan('darwin.dat'),ncol=3,byrow=T)
x<- xdat[,2]- xdat[,3]
  sig <- mean(abs(x - median(x)))   # scale estimate
  n <- length(x)
  tobs <- mean(x)
  x<- sort(x)

# parametric likelihood
th <-  seq(-1,7,len=40)
ff1 <- outer(th,x,'f1')

ll <- -n*log (c( ff1 %*% rep(1,n)))
ll<- ll-max(ll)
plot(th , ll,xlab=expression(theta),
     ylab='log-likelihood',type='n')
  lines(th, ll,lwd=.3 )
  abline(v= median(x),lwd=.3)

# empirical likelihood based on the mean
b <- meanlike(x)
lines(b[[1]], b[[2]], lty=2)
abline(v = mean(x),lty=2)
abline(h =-1.92,lwd=.1)

