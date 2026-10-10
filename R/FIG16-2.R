# Figure 16.2

N<-10
a<-floor(.6*N); b<-N-a
n<-8
  x<- 0:n
  y<- n-x


# likelihood solution 
fx<- lgamma(n+1) - lgamma(x+1)-lgamma(y+1) +
    (a+x)*log(a+x) + (b+y)*log(b+y) - (N+n)*log(N+n)
  fx<- exp(fx-max(fx))
  fx<- fx/sum(fx)

# plug-in solution
gx<- dbinom(x,n,p=a/N)

# Bayesian solution:
bx <- lgamma(n+1) + lgamma(a+x+1)+ lgamma(b+y+1)+lgamma(N+1)-
     lgamma(a+1) - lgamma(b+1) - lgamma(x+1) - lgamma(y+1) -
     lgamma(N+n+1)
bx<- exp(bx-max(bx))
bx<- bx/sum(bx)

plot(x,fx,xlab='y',ylab='Probability',
       ylim=range(c(fx,gx,bx)),type='n')
  lines(x,fx,lwd=.4)
  lines(x,gx,lty='dotted',lwd=1.5)
  lines(x,bx,lty='dashed')
  title(expression('Binomial prediction'))
