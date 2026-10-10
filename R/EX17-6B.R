# Example 17.6: to get Figure 17.3

# .........................  profile likelihood from ex17-6a.r
SIG <- seq(.001,.6,len=20)
PROF<-  c(-1.171406909, -1.106658507, -0.936678576, -0.702569256,
          -0.454335465, -0.236323992, -0.079413698,  0.000000000,
          -0.002852236, -0.085023753, -0.239262074, -0.456445355,
          -0.727120174, -1.042381450, -1.394319476 ,-1.776186584,
          -2.182371351, -2.608234921, -3.049836886, -3.503447502)

par(mfrow=c(2,2))
plot(SIG^2,PROF,xlab=expression(sigma[b]^2),
     ylab='Log-likelihood',type='n')
  lines(SIG^2,PROF)
  abline(v=0.236^2,h=log(.15),lwd=.3)

sig<- seq(0.001,.6,len=40)
  se <- (2*0.236)*0.11
  lnorm<- -1/2/se^2*(sig^2-0.236^2)^2
  lines(sig^2,lnorm,lty='dotted',lwd=1.52)
text(.3,-1.6,'15% cutoff',cex=.7)
title(expression(paste(sigma[b]^2,' scale')))

plot(SIG,PROF,xlab=expression(sigma[b]),
     ylab='Log-likelihood',type='n')
  lines(SIG,PROF)
  abline(v=0.236,h=log(.15),lwd=.3)
sig<- seq(0.001,.6,len=40)
  se <- 0.11
  lnorm<- -1/2/se^2*(sig-0.236)^2
  lines(sig,lnorm,lty='dotted',lwd=1.52)
title(expression(paste(sigma[b],' scale')))


plot(log(SIG),PROF,
     xlab=expression(paste('Log ',sigma[b])),
     ylab='Log-likelihood',type='n')
  lines(log(SIG),PROF)
  abline(v=log(0.236),h=log(.15),lwd=.3)
sig<- seq(0.001,.6,len=40)
  se <- 1/0.236*0.11
  lnorm<- -1/2/se^2*(log(sig)-log(0.236))^2
  lines(log(sig),lnorm,lty='dotted',lwd=1.52)
title(expression(paste('Log ',sigma[b],' scale')))
