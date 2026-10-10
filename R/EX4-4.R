
# Example 4.4

x<- 5; m<- 15
y<- 1; n<- 10

that<- log(x/(m-x)*(n-y)/y)
  se2<- 1/x + 1/(m-x) + 1/y + 1/(n-y)
  se<- sqrt(se2)

ehat<- log(y/(n-y))
  #print(c(that,ehat))
  cat('theta-hat=',that,',  se=',se,'\n')

# plots
par(mfrow=c(2,2))
th<- seq(that-2*se,that+3*se,len=30)
et<- seq(ehat-4,ehat+2,len=30)

fun1<- function(theta,eta){
      a <- exp(theta+eta)
      b<- exp(eta)
      x*theta + (x+y)*eta - m*log(1+a) - n*log(1+b)
      }

ll<- outer(th,et,'fun1')
like<- exp(ll-max(ll))
contour(th,et,like,level=c(.1,.3,.5,.7,.9),
        xlab=expression(theta),
        ylab=expression(eta))
  title(expression('(a) Likelihood contour'))
  abline(v=that,h=ehat,lwd=.4)

lik1<- apply(like,1,max)
plot(th,lik1,type='n',xlab=expression(theta),
     ylab='Likelihood')
  lines(th,lik1)
  abline(h=.15,lwd=.4)
  abline(v=0)
  title(expression('(b) Profile likelihood'))

# getting likelihood of H0
  a<- approx(th,lik1,xout=0)$y
  print(a)

# getting likelihood interval
  a <- li(th,lik1,0.15)
  cat('95% CI for theta=',a,'\n')



# ................... more extreme table
x<- 6; m<- 15
y<- 0; n<- 10

y<-0.5 # only to get estimates for plotting purpose
that<- log(x/(m-x)*(n-y)/y)
se2<- 1/x + 1/(m-x) + 1/y + 1/(n-y)
se<- sqrt(se2)

ehat<- log(y/(n-y))
  #  print(c(that,ehat))

th<- seq(that-2*se,that+3*se,len=30)
et<- seq(ehat-6,ehat+2,len=30)

y<-0 # the actual value of y
  ll<- outer(th,et,'fun1')
  like<- exp(ll-max(ll))
contour(th,et,like,level=c(.1,.3,.5,.7),
        xlab=expression(theta),
        ylab=expression(eta))
  title(expression('(c) Likelihood contour'))

lik1<- apply(like,1,max)
plot(th,lik1,type='n',xlab=expression(theta),
     ylab='Likelihood')
  lines(th,lik1)
  abline(h=.15,lwd=.4)
  abline(v=0)
  title(expression('(d) Profile likelihood'))

# likelihood of theta=0
a<- approx(th,lik1,xout=0)$y
  cat('Likelihood of theta=0',a,'\n')

# lower bound
source('li.r')
a<- li(th,lik1,0.15)
  cat('lower limit of 15% likelihood interval for theta=',a[1],'\n')

