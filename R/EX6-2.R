# Example 6.2

xdat<- scan('hanford.dat',skip=6,what=list(co='',0,0))
  x<- xdat[[2]]
  y<- xdat[[3]]
par(mfrow=c(2,2))


plot(x,y,xlab='Index of exposure',ylab='Cancer rate',type='n')
  points(x,y,cex=.5)
  title(expression('(a) Hanford data'))
  x0<- x-mean(x)
  xlm <- lm(y~ x0)
print(summary(xlm)$coef)

  bohat<- xlm$coef[1]
  b1hat<- xlm$coef[2]
  muhat <- bohat+ b1hat*x0
  lines(x,muhat)

n<-length(x)
fun1 <- function(bo,b1){
	-n/2*log(sum((y-bo-b1*x0)^2))
        }

bbo<- seq(bohat-10,bohat+10,len=20)
bb1 <- seq(b1hat-4,b1hat+4,len=20)
ll<- NULL
for (bo in bbo)
{
  for (b1 in bb1){
     ll <- c(ll,fun1(bo,b1))
  }
}
lik<- exp(ll-max(ll))
#
contour(bbo,bb1,matrix(lik,20,byrow=T),level=seq(.1,1,by=.2),
        xlab=expression(beta[0]),
        ylab=expression(beta[1]))
  title(expression('(b) Contour of the likelihood'))
  abline(h=b1hat,v=bohat,lwd=.3)

bb1<- seq(b1hat-4,b1hat+4,len=100)
fun2<- function(b1){
       bo<- mean(y)
      -n/2*log(sum((y-bo-b1*x0)^2))
        }
ll<-NULL
for (b1 in bb1){
     ll <- c(ll,fun2(b1))
  }
like<- exp(ll-max(ll))
plot(bb1,like,xlab=expression(beta[1]),
     ylab='Likelihood',type='n')
  lines(bb1,like,lwd=.4)
  abline(h=.15)
  title(expression(paste('(c) Profile for ',beta[1])))


a<-qqnorm(xlm$res,plot=F)
plot(a,xlab='Normal quantiles',ylab='Residuals',type='n')
  points(a,cex=.5)
  title(expression('(d) Normal plot of the residuals'))

