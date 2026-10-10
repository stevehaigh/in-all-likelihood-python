# Example 4.17

# heavy-tailed data example:

x<- c(-26.8,-3.5, -3.4, -1.2,  0.4, 1.3, 2.3, 2.7,
      3.0 , 3.2,  3.2,  3.5,  3.6, 3.9, 4.2, 4.4,
      5.0 , 6.5,  6.7,  7.1,  8.1, 10.5,10.7, 24.0, 32.8)

n<- length(x)

s2 <- var(x)*(n-1)/n
  llmax <- sum(log(dnorm(x,mean(x),sqrt(s2))))  # maximized likelihood
  cat('normal AIC=',(-2*llmax + 2*2),'\n')

par(mfrow=c(2,2))
a<-qqnorm(x,plot=F); 
plot(a,xlab='Quantiles of standard normal',
       ylab='x',type='n')
  qqline(x,lty=3)
  points(a,cex=.6,lwd=.5)
  title(expression('(a) Radiation data'),cex=2)

np<-100
xbar<- mean(x)
mu<- seq(xbar+ 5*sd(x)/sqrt(n),xbar- 5*sd(x)/sqrt(n) ,len=np)
fun1<- function(x,mu){ abs(x-mu)^2}
  xmu <- outer(x,mu,'fun1')
  nsig<- t(xmu) %*% rep(1,n)
  nsig<- c(nsig)
  p<-2
  loglik<- -n/p*log(nsig/n) - n/2
  m1 <- max(loglik)
  lik1<- exp(loglik-max(loglik))

plot(range(mu),range(lik1),type='n',
     xlab=expression(mu),
     ylab='Likelihood')
  lines(mu,lik1,lty='dotted',lwd=1.5)

# cauchy likelihood:
fun3 <- function(p){ # -loglike from cauchy
        mu <- p[1]
        sigma<- p[2]
        -sum(log(dcauchy(x,mu,sigma)))
       }
a<- nlm(fun3,p=c(0.3,1))
  cat('Cauchy muhat =',a$est,'\n')
  cat ('Cauchy AIC = ',(2*a$min + 4),'\n') 


# profile from cauchy model
# fun2 takes global x and mui
fun2 <- function(sigma){ # -loglike from cauchy
        -sum(log(dcauchy(x,mui,sigma)))
       }
ll<- NULL
for (i in 1:np){
   mui<- mu[i]
   lli<-nlm(fun2,1)$minimum
   ll<- c(ll,lli)
   }
#m2 <- -min(ll)
#  cat('Cauchy AIC=',(-2*m2 + 2*2),'\n')  # should be the same as above

ll<- exp(min(ll)-ll)
  lines(mu,ll,lty=1)
  title(expression(paste('(b) Profile likelihood of ',mu)))
  abline(h=.15,lwd=.2)



### ..................               more normal data

# speed of light data from Example 4.8
x<- c(890, 810, 810, 820, 800, 770, 760, 740, 750, 760,
    910, 920, 890, 860, 880, 720, 840, 850, 850, 780,
	890, 840, 780, 810, 760, 810, 790, 810, 820, 850,
    870, 870, 810 ,740, 810, 940, 950, 800, 810, 870)

# speed of light Michelson 1882, from Stigler 1977
#x <- c(883,816,778,796,682,711,611,599,1051,781,578,
#      796,774,820,772,696,573,748,748,797,851,809,723)

n<- length(x)
s2 <- var(x)*(n-1)/n
  llmax <- sum(log(dnorm(x,mean(x),sqrt(s2))))  # maximized likelihood
  cat('normal AIC=',(-2*llmax + 2*2),'\n')

a<-qqnorm(x,plot=F)
plot(a,xlab='Quantiles of standard normal',
       ylab='x',type='n')
  points(a,cex=.6,lwd=.5)
  qqline(x,lty=3)
  title(expression('(c) Michelson data'))

np<-100
xbar<- mean(x)
mu<- seq(xbar+ 5*sd(x)/sqrt(n),xbar- 5*sd(x)/sqrt(n) ,len=np)
fun1<- function(x,mu){ abs(x-mu)^2}
  xmu <- outer(x,mu,'fun1')
  nsig<- t(xmu) %*% rep(1,n)
  nsig<- c(nsig)
  p<-2
  loglik<- -n/p*log(nsig/n) - n/2
  m1 <- max(loglik)
  lik1<- exp(loglik-max(loglik))

plot(range(mu),range(lik1),type='n',
     xlab=expression(mu),
     ylab='Likelihood')
 lines(mu,lik1,lty='dotted',lwd=1.5)

# profile from cauchy model
# fun2 takes global x and mui
fun2 <- function(sigma){ # -loglike from cauchy
        -sum(log(dcauchy(x,mui,sigma)))
       }
ll<- NULL
for (i in 1:np){
   mui<- mu[i]
   lli<- nlm(fun2,1)$minimum
   ll<- c(ll,lli)
   }
cat('Cauchy AIC=',(2*min(ll) + 2*2),'\n')

ll<- exp(min(ll)-ll)
  lines(mu,ll,lty=1)
  title(expression(paste('(d) Profile likelihood of ',mu)))
  abline(h=.15,lwd=.2)

