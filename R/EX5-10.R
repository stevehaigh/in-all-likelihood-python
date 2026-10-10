
# Example 5.10
x<-3

th<- seq(0.01,10,len=100)
ll<- -th + x*log(th)
  like<- exp(ll-max(ll))
  like<- like/sum(like)/(th[2]-th[1])

right<- 1-ppois(x-1,th)
c1<- diff(right)/diff(th)

par(mfrow=c(2,2))
plot(th,right,xlab=expression(theta),
              ylab='Distribution',type='n')
  lines(th,right)
  title(expression('(a) Confidence distribution'))

low<- max(th[right<0.025])
up<- min(th[right>0.975])
print(c(low,up))

plot(th[-1],c1,xlab=expression(theta),
     ylab='Density',type='n')
  lines(th[-1],c1,lwd=.5)
  lines(th,like,lty=2)
  abline(v=c(low,up),lwd=.3)
  title(expression('(b) Confidence density'))
