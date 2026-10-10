
#  Figure 9.6:

n<- 5
est<- c(.35,.55,2.5,4.0)

par(mfrow=c(2,2))
for (i in 1:4){
   that <- est[i]
   sumx<- n/that
   th<- seq(that/5,that*4,len=100)
   lt<- -th*n/that + n*log(th)
   lt<- 2*(lt-max(lt))
   plot(th,lt,type='l',xlab=expression(theta),
         ylab='2*log-likelihood')
     abline(v=1,lwd=.2)
     abline(h=-qchisq(.9,1),lwd=.3)

# normal approx
  ihat <- n/that^2
  nl <- -ihat*(that-th)^2
  lines(th,nl,lty='dotted',lwd=1.5)
  title(substitute(hat(theta) == that, list(that=that)))
}

