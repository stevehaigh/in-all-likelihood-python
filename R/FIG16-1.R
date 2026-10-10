# Figure 16.1

par(mfrow=c(1,1))

N<-10
a<-floor(.6*N); b<-N-a
n<-8
  x<- 0:n
  y<- n-x
gx<- dbinom(x,n,p=a/N)   # plug-in solution

# Bayesian solution:
bx <- lgamma(n+1) + lgamma(a+x+1)+ lgamma(b+y+1)+lgamma(N+1)-
     lgamma(a+1) - lgamma(b+1) - lgamma(x+1) - lgamma(y+1) -
     lgamma(N+n+1)
bx<- exp(bx-max(bx))
bx<- bx/sum(bx)

plot(x,gx,xlab='y',ylab='Probability',
       ylim=range(c(gx,bx)),type='n')
  lines(x,gx,lty='dotted',lwd=1.5)
  lines(x,bx,lty='dashed')
  title(expression('Binomial prediction'))



