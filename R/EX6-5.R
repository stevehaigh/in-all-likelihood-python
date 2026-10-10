# Example 6.5

xy <- scan('claim.dat',what=list(x=0,y=0))
  x<- xy$x
  y<- xy$y

par(mfrow=c(2,2))
plot(x,y,xlab='Age',ylab='Number of claims',type='n')
  points(x,y,cex=.5)
  title(expression('(a) Claims data'))

# glm
  x0 <- x-mean(x)
  xglm <- glm(y~x0,family=poisson)
    b<- summary(xglm)$coef
    print(b)
    b0hat<-xglm$coef[1]; se0<- b[1,2]
    b1hat<-xglm$coef[2]; se1<- b[2,2]
  etahat <- xglm$coef[1] + xglm$coef[2]*x0
  muhat  <- exp(etahat)
  lines(x,muhat)

# 2d likelihood
x0 <- x-mean(x)
fun1 <- function(bo,b1){
	sum(y*(bo+b1*x0)- exp(bo+b1*x0))
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
  maxlik <- max(lik)
  lik <- lik- maxlik
  lik<- exp(lik)

contour(bbo,bb1,matrix(lik,20,byrow=T),level=seq(.1,1,by=.2),
        xlab=expression(beta[0]),
        ylab=expression(beta[1]))
  title(expression('(b) Contour of the likelihood'))
  abline(h=xglm$coef[2],v=xglm$coef[1],lwd=.3)

lik<- matrix(lik,20,byrow=T)
  lik1 <- apply(lik,2,max)
#lik1 <- lik1/sum(lik1)/(bb1[2]-bb1[1])
plot(bb1,lik1,type='n',xlab=expression(beta[1]),
     ylab='Likelihood')
  lines(bb1,lik1,lwd=.4)
  abline(h=.15,lwd=.3)
  source('li.r') 
  print(li(bb1,lik1,.15))
  title(expression('(c) Profile likelihood'))

# normal approx
ll<- log(lik1)
ll2<- -1/2/se1^2*(bb1-b[2,1])^2
plot(bb1,ll,type='n',xlab=expression(beta[1]),
     ylim=c(-4,0),
     ylab='log-likelihood')
  lines(bb1,ll,lwd=.5)
  lines(bb1,ll2,lty=2)
  title(expression('(d) Quadratic approximation'))
