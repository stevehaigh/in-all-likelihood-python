# Example 6.7

xdat<- matrix(scan('compete.dat',skip=1),ncol=4,byrow=T)
  y<-  xdat[,3]
  den<- xdat[,2]
  n<- length(y)

plot(den,y,xlab='Density',
     ylab='Yield', ylim=c(0,max(y)),type='n')
  points(den,y,cex=.7)
  title(expression('(a) Competition data'))

x0 <- log(den)-mean(log(den))
  a<- lm(1/y ~ x0)
  asum<- summary(a)
  print(asum$coef)

xglm<- glm(y~ x0, family=Gamma)  # gamma fit produces
                                # the same param est. with exponential fit
                                # but different std.err.
  lines(den,xglm$fit,lty=3,lwd=1.5)

plot(den,1/y,xlab='Density',ylab='1/Yield',
    type='n',log='x')
  points(den,1/y,cex=.7)
  title(expression('(b) Transformed data'))

# ................  glm
muhat<- xglm$fit
phi <- 1/(n-2)*sum((y-muhat)^2/muhat^2)   # ....... phi=0.025
  print(phi)  

b<- summary(xglm)$coef
b0hat<-xglm$coef[1]; se0<- b[1,2]/sqrt(0.025)
b1hat<-xglm$coef[2]; se1<- b[2,2]/sqrt(0.025)
cat('beta0-hat=',b0hat,',  se=',se0,'\n')
cat('beta1-hat=',b1hat,',  se=',se1,'\n')

fun1 <- function(bo,b1){
    a<- ifelse(((bo+b1*x0)<0.00001),0.00001,bo+b1*x0)
	sum(-y*a + log(a))
        }

lik<- NULL
bbo<- seq(b0hat-3*se0, b0hat+3*se0 ,len=20)
bb1 <- seq(b1hat-3*se1, b1hat+3*se1,len=20)
for (bo in bbo)
{
  for (b1 in bb1){
     lik <- c(lik,fun1(bo,b1))
  }
}
lik<- exp(lik- max(lik))

contour(bbo,bb1,matrix(lik,20,byrow=T),
        level=seq(.1,1,by=.2),
        xlab=expression(beta[0]),
        ylab=expression(beta[1]))
  title(expression('(c) Contour of the likelihood'))
  abline(h=xglm$coef[2],v=xglm$coef[1],lwd=.3)

lik<- matrix(lik,20,byrow=T)
  lik1 <- apply(lik,2,max)

plot(bb1,lik1,type='n',
    xlab=expression(beta[1]),
    ylab='Likelihood')
  lines(bb1,lik1,lwd=.4)
  abline(h=.15,lwd=.3)
  title(expression('(d) Profile likelihood'))
