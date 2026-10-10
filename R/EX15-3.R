#  Example 15.3

xdat<- matrix(scan('law.dat'),ncol=2,byrow=T)
  x<- (xdat[,1]); y<- (xdat[,2])
  n<- length(x)
  tobs <- cor(x,y)

bw<- .1        # smoothing parameter to estimate h(theta,t<-obs)
.Random.seed<- c(0,1900,90,1)

par(mfrow=c(2,2))
plot(x,y,xlab='SAT',ylab='GPA',type='n')
points(x,y,cex=.7)
title(expression('(a) Law school data'))

M1<- NULL
lik<- NULL
nb<-  200
nb2<- 100
for (i in 1:nb){
  id <- sample(1:n,replace=T)
    x1 <- x[id]; y1<- y[id]
  M1 <- c(M1,cor(x1,y1))
  id2 <- sample(1:n,size=n*nb2,replace=T)
      x2 <- matrix(x1[id2],ncol=nb2)
      y2 <- matrix(y1[id2],ncol=nb2)
      sx2 <- scale(x2)
      sy2 <- scale(y2)
      xy <- sx2*sy2
      cor2 <- c(t(xy) %*% rep(1,n))/(n-1)   # corrected version
    # density estimate at t=tobs
    ft <- sum(dnorm(cor2,mean=tobs,sd= bw))/nb2
    lik <- c(lik,ft)
}

# smoothing:
require(modreg)
a <- supsmu(M1,log(lik))
ll <- log(lik) - max(a$y)
plot(M1 , ll,xlab=expression(rho),
     ylab='log-likelihood',type='n')
  points(M1 , ll, cex=.4)
  title(expression('(b) Bootstrap likelihood'))
  ll <- a$y - max(a$y)  # smoothed loglik
  lines(a$x, ll,lwd=.3)
  abline(v=tobs,lwd=.3)

