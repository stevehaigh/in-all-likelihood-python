# Example 6.6

x<- scan('accident.dat',
        what=list(acc0=0,acc1=0,year0=0,year1=0))
  attach(x)

acc<- c(acc0,acc1)
year<- c(year0,year1)
site<- rep(1:8,2)
treat <- rep(c(0,1),c(8,8))

xreg<- glm(acc~ treat,offset=log(year),family=poisson)
  print(summary(xreg))


