# Figure 11.8

#reading data:
m1<-10   # active
m2<-12   # placebo
T12 <- scan('epi.dat',skip=2,nlines=1)
  T1<- T12[1:10]
  T2<- T12[11:22]
n12 <- scan('epi.dat',skip=4,nlines=1)
  n1<- n12[1:10]
  n2<- n12[11:22]
t1 <- scan('epi.dat',skip=7,nlines=2)
t2 <- scan('epi.dat',skip=10)


cat('y<-a and y<-p = ',sum(n1),sum(n2),'\n')
cat('Sum T<-i = ',sum(T1),sum(T2),'\n')

npar<-40
theta<- seq(0.2,.99,len=npar)
loglik <- 29*log(97*theta/(97*theta+109))+ 
         71*log(109/(97*theta+109))
like1 <- exp(loglik- max(loglik))

par(mfrow=c(2,2))
plot(range(theta),c(0,1),type='n',xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like1,lwd=.4)
  title(expression('(a) Simple likelihood'))
  that<- 29/97/71*109
  abline(v=that,lwd=.4)


# .....................poisson regression: 
y<- c(n1,n2)
x<- c(rep(1,m1),rep(0,m2))
off<- log(c(T1,T2))

xreg<- glm(y~ x+ offset(off),family=poisson)
print(summary(xreg))

fun1<- function(b0,b1){
  logmu <- off+ b0 + b1*x
  sum(-exp(logmu) + y* logmu)
 }

bb0<- seq(-.1,-.99,len=20)
bb<- log(theta)
loglik2<- NULL
for (b0 in bb0){
  for (b in bb){ loglik2<-c(loglik2,fun1(b0,b)) }
}

loglik2<- matrix(loglik2,ncol=npar,byrow=T)
  loglik2<- apply(loglik2,2,max)
  like2 <- exp(loglik2-max(loglik2))

plot(range(theta),c(0,1),type='n',xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like2,lwd=.8)
  title(expression('(b) From Poisson regression'))
  abline(v=that,lwd=.4)

# ......  Cox likelihood:
loglik3 <-  29*log(theta/(m1*theta+m2))+ 71*log(1/(m1*theta+m2))
  like30 <- exp(loglik3- max(loglik3))

plot(range(theta),c(0,1),type='n',xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like30,lwd=.3,lty='dotted',lwd=1.5)
  title(expression('(c) Cox likelihood'))
  abline(v=that,lwd=.4)

# better computation:
cutp<- 1:14
d1<- cut(T1,(cutp-.01))
  d1<- as.vector(table(d1))
  risk1<- c(m1,m1- cumsum(d1))
d2<- cut(T2,(cutp-.1))
  d2<- as.vector(table(d2))
  risk2<- c(m2,m2- cumsum(d2))
a1<- cut(t1,c(0,cutp))
  a1<- as.vector(table(a1))
a2 <- cut(t2,c(0,cutp))
  a2<- as.vector(table(a2))
  
fun2 <- function(th){
       sum(a1*log(th/(risk1*th+risk2)))+
         sum(a2*log(1 /(risk1*th+risk2)))
}
loglik3<-NULL
for (th in theta){ loglik3<- c(loglik3,fun2(th))}
like3 <- exp(loglik3- max(loglik3))
  lines(theta,like3,lwd=.4)
  abline(v=that,lwd=.4)

# all likelihoods:

plot(range(theta),c(0,1),type='n',xlab=expression(theta),
     ylab='Likelihood')
  lines(theta,like1,lwd=.3,lty=1)
  lines(theta,like2,lwd=.3,lty=2)
  lines(theta,like3,lwd=.3,lty=3)
  title(expression('(d) All likelihoods'))
  abline(v=that,lwd=.4)
