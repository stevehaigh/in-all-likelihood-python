# Figure 5.8


x<-3
th<- seq(0.01,10,len=400)
ll<- -th + x*log(th)
like<- exp(ll-max(ll))
like<- like/sum(like)/(th[2]-th[1])

right1<- 1-ppois(x-1,th)
c1<- diff(right1)/diff(th)
right2<- 1-ppois(x,th)
c2<- diff(right2)/diff(th)

par(mfrow=c(2,2))
plot(th,right1,xlab=expression(theta),
     ylab='Distribution',type='n')
  lines(th,right1,lwd=.5)
  lines(th,right2,lwd=.5,lty=2)
  title(expression('(a) Confidence distributions'))

plot(th[-1],c1,xlab=expression(theta), 
     ylab='Density',type='n')
  lines(th[-1],c1,lwd=.5)
  lines(th[-1],c2,lwd=.5,lty=2)
  abline(v=c(.62,8.76),lwd=.3)

title(expression('(b) Confidence densities'))


right1<- 1-ppois(x-1,th)
c1<- diff(right1)/diff(th)
right2<- 1-ppois(x,th)
c2<- diff(right2)/diff(th)
plot(th,right1,xlab=expression(theta),
     ylab='Distribution',type='n')
  lines(th,right1,lwd=.5,lty=2)
  lines(th,right2,lwd=.5,lty=2)

right<- 1-ppois(x-1,th) - 0.5*dpois(x,th)
  lines(th,right)

low<- max(th[right<0.025])
up<- min(th[right>0.975])
print(c(low,up))  # 95% confidence interval
title(expression('(c) Using mid-P-value'))

plot(th[-1],c1,xlab=expression(theta),
     ylab='Density',type='n')
  lines(th[-1],c1,lwd=.5,lty=2)
  lines(th[-1],c2,lwd=.5,lty=2)

right<- 1-ppois(x-1,th) - 0.5*dpois(x,th)
  c1<- diff(right)/diff(th)
  lines(th[-1],c1)

abline(v=c(low,up),lwd=.3)
title(expression('(d) Confidence densities'))

