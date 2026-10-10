
# Examples 2.3
# ....................................... Ex 2-3: (c) neg-binomial:

theta<- seq(.0001,.1,len=100)
like <- dbinom(1,53,theta)
  like1<- like/max(like)

like<- dbinom(5,552,theta)
  like2<- like/max(like)

plot(c(0,.1),range(c(like1,like2)),type='n',
     xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like1)
  lines(theta,like2,lty='dotted',lwd=1.5)
  title(expression('(c) Prevalence probability'))

