# Example 12.3

# waiting times between eruptions of
#  the famous Old Faithful geyser in Yellowstone National Park
  y<- scan('geyser.dat')
  n<- length(y)

par(mfrow=c(1,1))
h<- hist(y,nclass=20,prob=T,main='',
        xlab='Waiting time (minutes)',
        ylab='density',#inside=F,style='old',
        ylim=c(0,.05))

#starting values
  p<- 0.3
  mu1<- 55
  sig1<- 4
  mu2<- 80
  sig2<- 7

for (i in 1:25){
  deny <- p*dnorm(y,mu1,sig1) +  (1-p)*dnorm(y,mu2,sig2)
  p1 <-  p*dnorm(y,mu1,sig1)/deny
  p2 <-  1- p1

# updates:
  p <- sum(p1)/n
  mu1 <- sum(p1*y)/sum(p1)
  mu2 <- sum(p2*y)/sum(p2)
  sig11 <- sum(p1 *(y-mu1)^2)/ sum(p1); sig1<- sqrt(sig11)
  sig22 <- sum(p2 *(y-mu2)^2)/ sum(p2); sig2<- sqrt(sig22)
  cat(i,round(c(p,mu1,sig1,mu2,sig2),3),'\n')
}

  x<- seq(min(y),max(y),len=100)
  den <- p*dnorm(x,mu1,sig1) + (1-p)*dnorm(x,mu2,sig2)
  lines(x,den)
