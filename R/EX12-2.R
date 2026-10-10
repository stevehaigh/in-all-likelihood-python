# Example 12.2 and Figure 12.1
#  Illustration of EM algorithm:
 
y<- c(125,18,20,34)
phat<- 0.2
x<- c(0,0,18,20,34)
for (i in 1:5){
  x[1]<- .5/(.5+phat[i]/4)*y[1]
  x[2]<- y[1]-x[1]
  phat<- c(phat,(x[2]+x[5])/(sum(x)-x[1]))
}
print(round(phat,3))

p <- seq(.1,.9,len=40)
l1<- y[1]*log(.5+p/4) + (y[2]+y[3])*log(1-p) + y[4]*log(p)

par(mfrow=c(2,2))

for (i in 1:3){ 
  x[1]<- .5/(.5+phat[i]/4)*y[1]
  x[2]<- y[1]-x[1]
  l2<- (x[2]+x[5])*log(p) + (x[3]+x[4])*log(1-p)
  plot(p,l1-max(l1),type='l',xlab=expression(theta),
     ylab='Log-likelihood')
   lines(p,l2-max(l2),lty=2)
   abline(v=min(p[l2==max(l2)]),lty=2)
   abline(v=.627)
  if (i ==1) title(expression(paste('(a) ',theta^0,'= 0.2')))
  if (i ==2) title(expression(paste('(b) ',theta^1,'= 0.544')))
  if (i ==3) title(expression(paste('(c) ',theta^2,'= 0.615')))
}

lp<- y[1]*log(.5+phat/4) + (y[2]+y[3])*log(1-phat) + y[4]*log(phat)
  lp<- lp-max(lp)

plot(p,l1-max(l1),type='n',
      xlab=expression(theta),ylab='log-likelihood')
  lines(p,l1-max(l1),lwd=.3)
  points(phat,lp,cex=.7)
  text(phat,lp-4,0:4,cex=.8)
  abline(v=.627)
  title(expression('(d) Likelihood climb'))
