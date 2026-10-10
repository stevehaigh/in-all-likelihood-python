# Example 10.1: highly stratified data

mat<- matrix(scan('2way.dat'),ncol=3,byrow=T)
n <- nrow(mat)
  mu <- mat[,1]
  x<- mat[,2:3]
xmean <- as.vector(x %*% rep(.5,2))

sig2 <- seq(.2,2.2,len=60)
loglik <- -n*log(sig2) - sum((x-xmean)^2)/2/sig2

s2<- sum((x-xmean)^2)/(2*n)
llmax<- -n*log(s2) - sum((x-xmean)^2)/2/s2
s2<-1
ll1<--n*log(s2) - sum((x-xmean)^2)/2/s2

cat('support of sigma2=1',exp(ll1-llmax),'\n')
cat('RSS=',sum((x-xmean)^2),'\n')
  mlik <- max(loglik)
  like <- exp(loglik-mlik)

plot(sig2,like,xlab=expression(sigma^2),ylab='Likelihood',
     type='n')
  lines(sig2,like,lwd=.4)

# 'true' likelihood: using true means
loglik <- -n*log(sig2) - sum((x-mu)^2)/2/sig2
  mlik <- max(loglik)
  like4 <- exp(loglik-mlik)
#
  lines(sig2,like4,lty='dotted',lwd=1.5)
  abline(h=.15,lwd=.3)
  abline(v=1)
  title(expression("Profile and true likelihoods"))
