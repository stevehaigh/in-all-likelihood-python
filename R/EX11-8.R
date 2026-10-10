# Example 11.8

require(survival5)
xdat<- scan('rat.dat',skip=1,
           what=list(group=0,surv=0,status=0))
attach(xdat)
  t1 <- 100
  surv<- xdat$surv- t1

a<- coxph(Surv(surv,status)~ group,init=-.2)
  print(summary(a))

detach(xdat)
