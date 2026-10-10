# Example 11.5:
xdat<- scan('rat.dat',skip=1,
           what=list(group=0,surv=0,status=0))
attach(xdat)
  t1 <- 100
  surv<- xdat$surv- t1
  n1<- sum(group==1); n2<- sum(group==2)
  c1<- sum(group==1&status==0); c2<- sum(group==2 &status==0)
  x1 <- surv[group==1]; x2<-  surv[group==2]
detach(xdat)

# exponential likelihood

theta<- seq(50,350,len=100)

loglik1 <- -(n1-c1)*log(theta) - sum(x1)/theta
  lik1 <- exp(loglik1-max(loglik1))

loglik2 <- -(n2-c2)*log(theta) - sum(x2)/theta
  lik2 <- exp(loglik2-max(loglik2))

par(mfrow=c(2,2))
plot(theta,lik1,type='n',
     xlab=expression(theta),ylab='Likelihood')
  lines(theta,lik1,lwd=.5)
  lines(theta,lik2,lty='dotted',lwd=1.52)
  text(220,.8,'group2',cex=.7)
  text(80,.8,'group1',cex=.7)
  title(expression('(a) Exponential model'))

a <- -log(seq(1/34,by=1/17,len=17))
qqplot(a,x1[1:17],xlab='Exponential quantile',
    ylab='Ordered uncensored data',type='n')
  points(sort(a),sort(x1[1:17]),pch='1',cex=.6)
  a <- -log(seq(1/38,by=1/19,len=19))
  points(sort(a),sort(x2[1:19]),pch='2',cex=.6)
  title(expression('(b) Exponential check'))


# using normal model

sig2 <- (16*var(x1[1:17]) + 18*var(x2[1:19]))/34
sig <- sqrt(sig2)

loglik1<-NULL
for (thetai in theta){
   newl <- -sum((x1[1:17]-thetai)**2)/2/sig2
   loglik1 <- c(loglik1,newl)
}
lik1 <- exp(loglik1-max(loglik1))
for (i in 18:19){
  lik1<- lik1 * (1-pnorm(x1[i],theta,sig))
}
lik1 <- lik1/max(lik1)

#loglik2 <- -(n2)*log(theta) - sum(x2)/theta
loglik2<-NULL
for (thetai in theta){
   newl <- -sum((x2[1:19]-thetai)**2)/2/sig2
   loglik2 <- c(loglik2,newl)
}
lik2 <- exp(loglik2-max(loglik2))
for (i in 20:21){
  lik2<- lik2 * (1-pnorm(x2[i],theta,sig))
}
lik2 <- lik2/max(lik2)

plot(theta,lik1,type='n',xlab=expression(theta),
          ylab='Likelihood')
  lines(theta,lik1,lwd=.5)
  lines(theta,lik2,lty='dotted',lwd=1.5)
  text(185,.8,'group2',cex=.7)
  text(80,.8,'group1',cex=.7)
  title(expression('(c) Normal model'))

qqnorm(x1[1:17],xlab='Normal quantile',
    ylab='Ordered uncensored data',type='n',main='')
  a <- qnorm(seq(1/34,by=1/17,len=17))
  points(sort(a),sort(x1[1:17]),pch='1',cex=.6)
  a <- qnorm(seq(1/38,by=1/19,len=19))
  points(sort(a),sort(x2[1:19]),pch='2',cex=.6)
  title(expression('(d) Normality check'))
