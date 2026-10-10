# Example 6.19: stack loss versus cooling temperature

x<-  scan('stack.dat',skip=11)
  x<-  matrix(x,ncol=4,byrow=T)
  flow<-  x[,1]
  cooling<-  x[,2]
  concen<-  x[,3]
  loss<-  x[,4]

y<-  loss
x<-  cooling
n<-  length(x)
x0<-  x-mean(x)

# normal model:
a<-  lm(y~x)
  print(summary(a)$coef)

par(mfrow=c(2,2))
plot(x,y, xlab='Cooling temperature',ylab='Loss',type='n')
  text(x,y,c(1:n),cex=.7)
  lines(x,a$fit)
  title(expression('(a) Stack-loss data'))

# AIC:
       bo<-  a$coef[1]
       b1<-  a$coef[2]
       sigma<-  sqrt(sum(a$resid^2)/n)
       mu<-  bo+ b1*x
       aic <-  -2*sum(log(dnorm(y,mu,sigma))) + 6  # normal model
       cat('Normal AIC= ', aic, ' \n')

fun1 <-  function(p){
        bo<-  p[1]
        b1<-  p[2]
        sigma<-  p[3]
        mu<-  bo+ b1*x
       -sum(log(dcauchy(y,mu,sigma)))  # cauchy model
       }
ams<-  nlm(fun1,c(-20,1,2))
cat('Cauchy AIC= ',(2*ams$min+6),'\n')
cat('Cauchy-based estimates for beta0, beta1, sigma',ams$est,'\n')

mu<-  ams$est[1] + ams$est[2] * x
  lines(x,mu,lty='dotted',lwd=1.5)
  print(ams$est)
  amsres <-  y-mu

b<-  qqnorm(a$res,plot=F)
plot(b,
     xlab='Standard normal quantiles',
     ylab='Residuals',type='n')
  bb<-  cbind(b$x,b$y)
  points(bb[-c(1:4),],cex=.7)
  text(bb[1:4,1],bb[1:4,2],c(1:4),cex=.7)
  qqline(a$res)
  title(expression('(b) Normal residuals'))

qqres<-  qqnorm(amsres,plot=F)
plot(qqres$x,qqres$y,
     xlab='Standard normal quantiles',
     ylab='Residuals',type='n')
  bb<-  cbind(qqres$x,qqres$y)
  points(bb[-c(1:4),],cex=.7)
  text(bb[1:4,1],bb[1:4,2],c(1:4),cex=.7)
#text(qqres,c(1:n),cex=.7)
   qqline(amsres)
title(expression('(c) Cauchy residuals'))

b<-  summary(a)$coef
  b1hat<-  b[2,1]; se<-  b[2,2]
bb1<-  seq(b1hat-3*se,b1hat+3*se,len=40)

fun2<-  function(b1){
       bo<-  mean(y) - b1*mean(x)
      -n/2*log(sum((y-bo-b1*x)^2))
        }
ll<- NULL
for (b1 in bb1){
     ll <-  c(ll,fun2(b1))
  }
ll<-  ll-max(ll)
like<-  exp(ll-max(ll))


fun1 <-  function(p){
        bo<-  p[1]
        sigma<-  p[2]
        mu<-  bo+ b1*x0
       -sum(log(dcauchy(y,mu,sigma)))  # cauchy model
       }
llc<-  NULL
bbc<-  seq(ams$est[2]-4*se,ams$est[2]+6*se,len=40)
for (b1 in bbc){
   lli<- nlm(fun1,c(17,1))$minimum
   llc<-  c(llc,lli)
   }
llc<-  (min(llc)-llc)
likec<-  exp(llc)

plot(bb1,like,xlab=expression(beta[1]),
     ylab='Likelihood',
     xlim=range(bbc),type='n')
lines(bb1,like,lwd=.4)
abline(h=.15)

lines(bbc,likec,lty='dotted',lwd=1.52)  # cauchy model
title(expression(paste('(d) Profile for ',beta[1])))


 info<- -diff(llc,dif=2)/(bbc[2]-bbc[1])^2
 aa<-  bbc[c(-1,-40)]
 info<-  approx(aa,info,xout=ams$est[2])$y
 sec<-  sqrt(1/info)
 cat('Standard errors of beta0-hat and beta1-hat',se,sec,'\n')



