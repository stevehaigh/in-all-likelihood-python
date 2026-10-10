
# Examples 2.4:
# ....................................... Ex 2-4: (d) normal example

theta <- seq(-1,6,len=40)

like <- pnorm(4,theta) - pnorm(0.9,theta)
  like <- like/max(like)
plot(theta,like,type='n',
     xlab=expression(theta),
     ylab='Likelihood')
lines(theta,like)

like <- dnorm(2.45,theta)
  like<- like/max(like)
  lines(theta,like,lty='dashed')

n<- 5
xn<- 3.5
llike <- log(dnorm(xn,mean=theta)) + (n-1)*log(pnorm(xn,mean=theta))
  like <- exp(llike-max(llike))
  lines(theta,like,lty='dotted',lwd=1.5)

title(expression('(d) Normal mean examples'))

