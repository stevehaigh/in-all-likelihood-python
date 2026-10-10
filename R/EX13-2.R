# Example 13.2

mu<- seq(-3,3,len=100)
  sig<- 1
  a<- (log(sig)+(mu^2+1)/sig-1)/2

par(mfrow=c(2,2))
plot(mu,a,type='n',xlab=expression(mu),
    ylab='KL distance')
  lines(mu,a,lwd=.5)
  title(expression(paste('(a) True=N(0,1), Model=N(',mu,',1)')))
  abline(h=0,lwd=.2)


plot(mu,a,type='n',xlab=expression(mu),
   ylab='KL distance')
 lines(mu,a,lwd=.5)
  text(-2.7,4,'1',cex=.7)
  abline(h=0,lwd=.2)
 title(expression(paste('(b) True=N(0,1), Model=N(',mu,',',sigma^2,')')))
sig<-2
  lines(mu,(log(sig)+(mu^2+1)/sig-1)/2,lty='dotted',lwd=1.5)
  text(-2.7,2.4,'2',cex=.7)
sig<-10
  lines(mu,(log(sig)+(mu^2+1)/sig-1)/2,lty='dotted',lwd=1.52)
  text(-2.7,.7,'10',cex=.7)
sig<-.5
  lines(mu,(log(sig)+(mu^2+1)/sig-1)/2,lty='dotted',lwd=1.52)
  text(-1.6,4,'0.5',cex=.7)
sig<-.1
lines(mu,(log(sig)+(mu^2+1)/sig-1)/2,lty='dotted',lwd=1.52)
text(-0,4,'0.1',cex=.7)
