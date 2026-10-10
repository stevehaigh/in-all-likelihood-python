# Example 5.17
# checking the coverage of x(1) to x(2)

.Random.seed<- c(0,1900,90,9)   # seed
n<- 100
  mu<-1
  x<- matrix(rnorm(n*2),ncol=2) + mu
  xmin<- apply(x,1,min)
  xmax<- apply(x,1,max)
  width<- xmax-xmin

ord<- order(width)
  xmin<- xmin[ord]
  xmax<- xmax[ord]


par(mfrow=c(2,2))
plot(c(1,n),range(x),xlab='Trials',ylab='Interval',type='n')
abline(h=mu)
for (i in 1:n){
   lines(c(i,i),c(xmin[i],xmax[i]),lwd=.4)
}
title(expression('(a) 50% confidence intervals'))

width<- xmax-xmin
  cv <- width/sqrt(2)/abs(xmin+xmax)

cover<- (xmin < mu ) & (mu< xmax)
plot(cv[cv<2],cover[cv<2],
    xlab='Coefficient of variation',
    ylab='Coverage',type='n')
  points(cv[cv<2],cover[cv<2],cex=.4)
  lines(lowess(cv[cv<2],cover[cv<2]),lwd = .5)
  abline(h=0.5)
  abline(v=sqrt(2)/(1+sqrt(2)))
  title(expression('(b) Coverage probability'))

