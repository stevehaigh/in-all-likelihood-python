# Example 15.2

source('meanlike.r')  # to compute empirical likelihood for the mean
par(mfrow=c(1,1))

x<- c(50, 44, 102, 72, 22, 39, 3, 15,
    197, 188, 79, 88, 46, 5, 5, 36,
    22, 139, 210, 97, 30, 23, 13, 14)
n<- length(x)
tobs <- mean(x)

bw<- 5       # smoothing parameter to estimate h(theta,t<-obs)
.Random.seed<- c(0,1900,90,91)   # Wichmann-Hill generator

M1<- NULL
lik<- NULL
nb<- 200
for (i in 1:nb){
  x1 <- sample(x,replace=T)
  M1 <- c(M1,mean(x1))
    x2 <- sample(x1,size = nb*n, replace=T)
    x2 <- matrix(x2,ncol=n)
    m2 <- c(x2 %*% rep(1,n))/n
    # density estimate at t=tobs
    ft <- sum(dnorm(m2,mean=tobs,sd= bw))/nb
    lik <- c(lik,ft)
}

# .................smoothing:
require(modreg)
a <- supsmu(M1,log(lik))
ll <- log(lik) - max(a$y)
plot(M1 , ll,xlab=expression(theta),
     ylab='Log-likelihood',type='n')
  points(M1 , ll, cex=.4)
  title(expression('Bootstrap likelihood'))
  ll <- a$y - max(a$y)  # smoothed loglik
  lines(a$x, ll,lwd=.2)


# empirical likelihood for the mean
b <- meanlike(x)
  lines(b[[1]], b[[2]], lty=2)
  abline(v = mean(x))


