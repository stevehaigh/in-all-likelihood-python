
# Examples 2.2:
# .......................................Ex 2-2: (b) badgers
n<- 25
k<- 60
n1<- 5

NN<- c(seq(150,200),seq(200,725,by=25),
      seq(725,775),seq(775,1100,by=25))
loglik<-NULL

for (N in NN){
  ll <-  lgamma(N-n+1) - lgamma(N-n-k+n1+1) -
           lgamma(N+1) + lgamma(N-k+1)
  loglik <- c(loglik,ll)
}
like<- exp(loglik-max(loglik))

plot(NN,like,type='l',xlab='N',ylab='Likelihood')
  #abline(h=.15,lwd=.3)
  title(expression('(b) The number of badgers'))

