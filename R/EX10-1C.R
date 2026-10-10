# Example 10.1 continued:

mat<- matrix(scan('2way.dat'),ncol=3,byrow=T)
n <- nrow(mat)
  mu <- mat[,1]
  x<- mat[,2:3]
xmean <- as.vector(x %*% rep(.5,2))

sig2 <- seq(.2,2.2,len=60)
loglik <- -n*log(sig2) - sum((x-xmean)^2)/2/sig2
  mlik <- max(loglik)
  like <- exp(loglik-mlik)

plot(sig2,like,xlab=expression(sigma^2),ylab='Likelihood',
     type='n')
lines(sig2,like,lwd=.4)

# marginal likelihood
y <- (x[,1]-x[,2])/sqrt(2)
loglik2 <- -n/2*log(sig2) - sum(y^2)/2/sig2
  mlik2 <- max(loglik2)
  like2<- exp(loglik2 - mlik2)
  lines(sig2,like2,lty=2,lwd=.4)

# true likelihood: using true means
loglik <- -n*log(sig2) - sum((x-mu)^2)/2/sig2
  mlik <- max(loglik)
  like4 <- exp(loglik-mlik)
#
  lines(sig2,like4,lty='dotted',lwd=1.5)
  abline(h=.15,lwd=.3)
  abline(v=1)
title(expression("Profile, marginal and true likelihoods"))

