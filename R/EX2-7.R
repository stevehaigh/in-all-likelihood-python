

par(mfrow=c(1,1))
theta <- seq(0,6,len=100)

# sample 1: data max=3.5 from x1 to x5 iid N(theta,1),
n<- 5
  x <- 3.5
  loglik1 <- (n-1)*log(pnorm(x-theta)) + log(dnorm(x-theta))
  loglik1 <- loglik1 - max(loglik1)
  lik1 <- exp(loglik1)

# sample2: ybar from y1 to y3 iid N(theta,1)
n<- 3
  ybar<- 4
  loglik2 <- log (dnorm(ybar,mean=theta,sd=sqrt(1/n)))
  loglik2 <- loglik2 - max(loglik2)
  lik2 <- exp(loglik2)
# combined sample
loglik <- loglik1 + loglik2
  loglik <- loglik- max(loglik)
  lik <- exp(loglik)

plot(theta,lik,type='l',xlab=expression(theta),
    ylab='Likelihood')
  lines(theta,lik1,lty=2)
  lines(theta,lik2,lty=3)
title(expression('Combining likelihoods'))

