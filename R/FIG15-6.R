# Figure 15.6
# 

source('meanlike.r')

# aircraft failure data 
x<- c(50, 44, 102, 72, 22, 39, 3, 15,
    197, 188, 79, 88, 46, 5, 5, 36,
    22, 139, 210, 97, 30, 23, 13, 14)
  n<- length(x)

f1<- function(lambda,x){
     x*exp(lambda*x)
    }
f2 <- function(lambda,x){
     exp(lambda*x)
    }
f3 <- function(lambda,x){
     lambda*x
    }
#
# 
sx <- c(scale(sort(x)))/sqrt(n)
lambda<- seq(-.05,.05,len=100)  # need to manipulate lambda properly
                               # so that theta covers
                               # enough parameter space around xbar
  ff1 <- outer(lambda,x,'f1')
  theta <- c(ff1 %*% rep(1,n))
  ff2 <- outer(lambda,x,'f2')
  sumexp <- c(ff2 %*% rep(1,n))
  theta<- theta/ sumexp
Ka <- log(sumexp/n)
sadll <- n*lambda*mean(x) - n*Ka   # alternative likelihood

# direct computation of likelihood:
   ff3 <- outer(lambda,x,'f3')
   dirll <- t(ff3 %*% rep(1,n)) - n*log(sumexp)
   dirll<- dirll - max(dirll)  # should be the same as sadll

a<- meanlike(x)    # empirical likelihood
plot(a[[1]],a[[2]],type='n', 
     xlab=expression(theta),ylab='Log-likelihood',
     ylim=c(-8,0),xlim=c(30,105))
  lines(theta,dirll,lwd=.4)   # saddlepoint approximation
  lines(a[[1]],a[[2]],lty=2)
  abline(v=mean(x))

