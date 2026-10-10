# Example 6.19: stack loss versus air flow

x<- scan('stack.dat',skip=11)
  x<- matrix(x,ncol=4,byrow=T)
  flow<- x[,1]
  cooling<- x[,2]
  concen<- x[,3]
  loss<- x[,4]

y<- loss
x<- flow
n<- length(x)
x0<- x-mean(x)

a<- lm(y~x)
  print(summary(a)$coef)

par(mfrow=c(2,2))
plot(x,y, xlab='Flow',ylab='Loss',type='n')
points(x,y,cex=.7)
  lines(x,a$fit)
  title(expression('(a) Stack-loss data'))

# log-likelihood:
       bo<- a$coef[1]
       b1<- a$coef[2]
       sigma<- sum(a$resid^2)/n
       mu<- bo+ b1*x
       aic <- -2*sum(log(dnorm(y,mu,sigma))) + 6  # normal model
       cat('Normal AIC= ', aic, ' \n')

fun1 <- function(p){
        bo<- p[1]
        b1<- p[2]
        sigma<- p[3]
        mu<- bo+ b1*x
       -sum(log(dcauchy(y,mu,sigma)))  # cauchy model
       }
ams<- nlm(fun1,c(-44,1,1))
cat('Cauchy-based estimates for beta0, beta1, sigma',ams$est,'\n')
cat('Cauchy AIC= ',(2*ams$min+6),'\n')


mu<- ams$est[1] + ams$est[2] * x
  lines(x,mu,lty='dotted',lwd=1.5)
  print(ams$est)

b<- qqnorm(a$res,plot=F)
plot(b,xlab='Standard normal quantiles',
     ylab='Residuals',type='n')
points(b,cex=.7)
 qqline(a$res)
title(expression('(b) Heavy-tailed residuals'))

b<- summary(a)$coef
   b1hat<- b[2,1]; se<- b[2,2]
bb1<- seq(b1hat-3*se,b1hat+3*se,len=40)

fun2<- function(b1){
       bo<- mean(y) - b1*mean(x)
      -n/2*log(sum((y-bo-b1*x)^2))
        }
ll<-NULL
for (b1 in bb1){
     ll <- c(ll,fun2(b1))
  }
ll<- ll-max(ll)
  like<- exp(ll-max(ll))
plot(bb1,like,xlab=expression(beta[1]),
     ylab='Likelihood',type='n')
  lines(bb1,like,lwd=.4)
  abline(h=.15)
  title(expression(paste('(c) Profile for ',beta[1])))

fun1 <- function(p){
        bo<- p[1]
        sigma<- p[2]
        mu<- bo+ b1*x0
       -sum(log(dcauchy(y,mu,sigma)))  # cauchy model
       }
llc<- NULL
for (b1 in bb1){
   lli<- nlm(fun1,c(17,1))$minimum
   llc<- c(llc,lli)
   }
  llc<- (min(llc)-llc)
  likec<- exp(llc)
  lines(bb1,likec,lty='dotted',lwd=1.5)

info<--diff(llc,dif=2)/(bb1[2]-bb1[1])^2
 aa<- bb1[c(-1,-40)]
 info<- approx(aa,info,xout=0.97)$y
 sec<- sqrt(1/info)
 cat('Standard errors of beta0-hat and beta1<-hat',se,sec,'\n')

plot(bb1,ll,xlab=expression(beta[1]),
   ylab='Log-likelihood',type='n')
  lines(bb1,llc,lty='dotted',lwd=1.5)
  ll2<- -1/2/sec^2*(bb1-ams$est[2])^2
  lines(bb1,ll2,lty='dashed')
  title(expression('(d) Quadratic appoximation'))

