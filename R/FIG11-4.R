# Figure 11.4

require(survival5)

xdat<- scan('rat.dat',skip=1,
           what=list(group=0,surv=0,status=0))

attach(xdat)
  est <- survfit(Surv(surv,status)~group)
  par(mfrow=c(1,1))
  plot(est,lty=1:2,xlab='Days',ylab='Survival probability',cex=0)
    title(expression('Kaplan-Meier estimates'))
detach(xdat)
