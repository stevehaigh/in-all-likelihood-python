#     Example 5.3

n<- 10
nrep<-1000
xbar<- 1.4   # the only value needed from the data

set.seed(6)
x<- matrix(rpois(nrep*n,xbar),ncol=10)
xmean <- x %*% rep(1,n)/n
xmean<- c(xmean)

par(mfrow=c(1,1))
hist(exp(-xmean),
     nclass=20,
     xlab=expression(paste(exp,'(',-bar(x),'*)')),
     main='')
    
title('Bootstrap distribution of exp(-xbar)',cex=.4)
