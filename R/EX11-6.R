# Example 10.6

require(survival5)
xdat<- scan('rat.dat',skip=1,
           what=list(group=0,surv=0,status=0))
attach(xdat)
 
t1 <- 100
  surv<- xdat$surv- t1

reg1<- survreg(Surv(surv,status)~factor(group), dist='exponential')
  print(summary(reg1))

reg2<- survreg(Surv(surv,status)~factor(group), dist='weibull')
  print(summary(reg2))

# Weibull vs exponential: LRT=44.8
cat('Weibull vs Exponential: LRT= ',2*(reg2$loglik[2] - reg1$loglik[2]),'\n')

detach(xdat)
