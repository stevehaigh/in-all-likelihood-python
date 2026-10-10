# ......Figure 13.1

x<- matrix(scan('darwin.dat'),ncol=3,byrow=T)
x<- x[,2]-x[,3]

a<-qqnorm(x,plot=F)
par(mfrow=c(2,2))
plot(a,xlab='Normal quantiles',ylab='Observed quantiles',type='n')
  points(a,cex=.7)
  qqline(x,lty='dotted')
  title(expression("(a) Normal plot"))

n<- length(x)
  mu<- seq(min(x),max(x),len=100)
  xbar<- mean(x)

fun<- function(x,mu,p){ abs(x-mu)^p}

p<-1
xmu <- outer(x,mu,'fun',p=p)
  nsig<- t(xmu) %*% rep(1,n)
  nsig<- c(nsig)
  co<- 1/gamma(1+1/p)/2^(1+1/p)  # constant term
  loglik<- n*log(co) -n/p*(log(nsig/n*p/2)) -n/p
  m1<- max(loglik)
  lik<- exp(loglik-max(loglik))
  lik1<- lik
print('support for zero')
print(approx(mu,lik1,xout=0)$y)


p<-2
xmu <- outer(x,mu,'fun',p=p)
  nsig<- t(xmu) %*% rep(1,n)
  nsig<- c(nsig)
  co<- 1/gamma(1+1/p)/2^(1+1/p)  # constant term
  loglik<- n*log(co) -n/p*(log(nsig/n*p/2)) -n/p
  m2<- max(loglik)
  lik<- exp(loglik-max(loglik))
  lik2<- lik
print('support for zero')
print(approx(mu,lik2,xout=0)$y)


p<-4
xmu <- outer(x,mu,'fun',p=p)
  nsig<- t(xmu) %*% rep(1,n)
  nsig<- c(nsig)
  co<- 1/gamma(1+1/p)/2^(1+1/p)  # constant term
  loglik<- n*log(co) -n/p*(log(nsig/n*p/2)) -n/p
  m4<- max(loglik)
  lik<- exp(loglik-max(loglik))
  lik4<- lik

p<-8
xmu <- outer(x,mu,'fun',p=p)
  nsig<- t(xmu) %*% rep(1,n)
  nsig<- c(nsig)
  co<- 1/gamma(1+1/p)/2^(1+1/p)  # constant term
  loglik<- n*log(co) -n/p*(log(nsig/n*p/2)) -n/p
  m8<- max(loglik)
  lik<- exp(loglik-max(loglik))
  lik8<- lik

plot(range(mu),range(c(lik1,lik2,lik4,lik8)),type='n',
     xlab=expression(mu),
     ylab='Profile likelihood')
  lines(mu,lik1,lty=1,lwd=.4)
  lines(mu,lik2,lty=2)
  points(x,rep(0,length(x)),cex=.5,pch='|')
  lines(mu,lik4,lty=3,lwd=.4)
  lines(mu,lik8,lty=4,lwd=.4)
  title(expression(paste('(b) ',L[p],' likelihood, p=1,2,4,8')))

# profile over p
cat('-Log L(p), in page 367:  ',-m1,-m2,-m4,-m8,'\n')


