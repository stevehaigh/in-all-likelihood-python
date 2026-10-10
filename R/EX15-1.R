# Example 15.1, including Figure 15.1

x<- c( 2.3, 2.4,  4.5, 5.4, 10.1); 
   n<- length(x)

f1<- function(lambda,x){
     x/(n-lambda*x)
    }

# trying a number of theta
th<- c(3,4,mean(x),6,7)
theta<- NULL
loglik<- NULL
for (i in 1:length(th)){
  a<- x-th[i]
  lam <- seq(n/min(a),n/max(a),len=22)
  lam <- lam[c(-1,-22)]
  glam <-outer(lam,a,'f1')
  glam<- c(glam %*% rep(1,n))
  lam0<- approx(glam,lam,xout=0)$y
  if (is.na(lam0)==F) {   # lam0 exists
     w <- 1/(n-lam0*a)
     ll<- sum(log(w))
       cat(th[i],round(lam0,2),round(w,2),round(ll,2),'\n')
     theta<- c(theta,th[i])
     loglik <- c(loglik,ll)
  }
}

# Figure 15.1
# g-lambda
th<- 3
  a<- x-th
  lam <- seq(n/min(a),n/max(a),len=22)
  lam <- lam[c(-1,-22)]
  glam <-outer(lam,a,'f1')
  glam<- c(glam %*% rep(1,n))
    lam0<- approx(glam,lam,xout=0)$y

par(mfrow=c(2,2))
plot(lam,glam,xlab=expression(lambda),
     ylab=expression(g(lambda)),type='l')
  abline(h=0, v=lam0,lty=2)
title(expression(paste('(a) Solving for ',lambda)))

# 
x<- sort(x)  # be sure to use sorted x here
nt<- 40
th<- seq(1.01*x[1],(x[n-1]+.99*(x[n]-x[n-1])),len=nt)

theta<- NULL
loglik<- NULL
for (i in 1:nt){
  a<- x-th[i]
  lam <- seq(n/min(a),n/max(a),len=22)
  lam <- lam[c(-1,-22)]
  glam <-outer(lam,a,'f1')
  glam<- c(glam %*% rep(1,n))
  lam0<- approx(glam,lam,xout=0)$y
  if (is.na(lam0)==F) {   # lam0 exists
     w <- 1/(n-lam0*a)
     ll<- sum(log(w))
     theta<- c(theta,th[i])
     loglik <- c(loglik,ll)
  }
}

lik<- exp(loglik-max(loglik))
plot(th,lik,type='l',xlab=expression(theta),
     ylab='Likelihood')
  title(expression('(b) Empirical likelihood'))
  abline(h=0.15,lwd=.2)
  text(9,.2,'95% CI',cex=.7)

