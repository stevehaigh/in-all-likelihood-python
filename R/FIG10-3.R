# Figure 10.3
par(mfrow=c(2,2))


x<- 5; m<- 15
y<- 1; n<- 10

that<- log(x/(m-x)*(n-y)/y)
se2<- 1/x + 1/(m-x) + 1/y + 1/(n-y)
se<- sqrt(se2)

ehat<- log(y/(n-y))
print(c(that,ehat))

th<- seq(that-3*se,that+4*se,len=20)
et<- seq(ehat-4,ehat+2,len=20)

fun1<- function(theta,eta){
      a <- exp(theta+eta)
      b<- exp(eta)
      x*theta + (x+y)*eta - m*log(1+a) - n*log(1+b)
      }

ll<- outer(th,et,'fun1')
  like<- exp(ll-max(ll))
  lik1<- apply(like,1,max)   # profile likelihood

plot(th,lik1,type='n',xlab=expression(theta),ylab='Likelihood')
  lines(th,lik1)
  abline(h=.15,lwd=.4)
  abline(v=0)
  title(expression('(a) 5/15 versus 1/10'))

# ...............     hypergeometric computation:
xx<- 0:(x+y)
  numx<- dhyper(x,m,n,(x+y))
  numxx<- dhyper(xx,m,n,(x+y))
l1 <- NULL
np<-40; th<- seq(that-3*se,that+4*se,len=np)
for (i in 1:np){
    theta<- th[i]; 
    px <- numx*exp(theta*x)
    pxx<- numxx*exp(theta*xx)
    li<- px/sum(pxx)
    l1<- c(l1,li)
}
l1<- l1/max(l1)
lines(th,l1,lty='dotted',lwd=1.52)


# ........................       second example: an extreme case
x<- 6; m<- 15
y<- 0; n<- 10

y<-.5   # only to compute estimates
that<- log(x/(m-x)*(n-y)/y)
se2<- 1/x + 1/(m-x) + 1/y + 1/(n-y)
se<- sqrt(se)

ehat<- log(y/(n-y))
print(c(that,ehat))

th<- seq(that-4*se,that+4*se,len=20)
et<- seq(ehat-6,ehat+2,len=20)

y<-0 # the actual value of y
ll<- outer(th,et,'fun1')
  like<- exp(ll-max(ll))
  lik1<- apply(like,1,max)   # profile likelihood

plot(th,lik1,type='n',
     xlab=expression(theta),ylab='Likelihood')
  lines(th,lik1)
  abline(h=.15,lwd=.4)
  abline(v=0)
  title(expression('(b) 5/15 versus 0/10'))

# hypergeometric computation:
xx<- 0:(x+y)
  numx<- dhyper(x,m,n,(x+y))
  numxx<- dhyper(xx,m,n,(x+y))
l1 <- NULL
np<-40; th<- seq(that-3*se,that+4*se,len=np)
for (i in 1:np){
    theta<- th[i]; 
    px <- numx*exp(theta*x)
    pxx<- numxx*exp(theta*xx)
    li<- px/sum(pxx)
    l1<- c(l1,li)
}
l1<- l1/max(l1)
lines(th,l1,lty='dotted',lwd=1.52)


