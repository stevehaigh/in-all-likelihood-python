# Figure 9.1: central limit theorem

set.seed(13)
nn<- c(3,20)
nrep<- 400

par(mfcol=c(2,3))
for (i in 1:3){
for (n in nn){
  if (i==1) x<- matrix(runif(n*nrep),ncol=n)
  if (i==2) x<- matrix(rexp(n*nrep),ncol=n)
  if (i==3) x<- matrix(rpois(n*nrep,3),ncol=n)
  xmean <- c(x %*% rep(1,n))/n 
  a<- qqnorm(xmean,type='n',ylab='Sample means',
    xlab='Normal quantiles',main='')
    qqline(xmean,lty=2)
    points(a,cex=.4)
    if (i==1) title(paste('Uniform n =',n))
    if (i==2) title(paste('Exponential n =',n))
    if (i==3) title(paste('Poisson n =',n))
}}
