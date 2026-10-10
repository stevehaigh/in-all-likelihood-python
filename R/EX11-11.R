# Example 11.11
# reading data:
m1<-10   # active
m2<-12   # placebo
T12 <- scan('epi.dat',skip=2,nlines=1)
  T1<- T12[1:10]
  T2<- T12[11:22]
n12 <- scan('epi.dat',skip=4,nlines=1)
  n1<- n12[1:10]
  n2<- n12[11:22]
t1 <- scan('epi.dat',skip=7,nlines=2)
t2 <- scan('epi.dat',skip=10)

# cutting into intervals
cutp<- (1:15)-.01
  d1<- cut(T1,(cutp-.01))
  d1<- as.vector(table(d1))
  risk1<- c(m1,m1- cumsum(d1))
d2<- cut(T2,(cutp-.1))
  d2<- as.vector(table(d2))
  risk2<- c(m2,m2- cumsum(d2))

a1<- cut(t1,c(-.1,cutp))
  a1<- as.vector(table(a1))
a2 <- cut(t2,c(-.1,cutp))
  a2<- as.vector(table(a2))
  

# data: ...............................Table 11.2
a<- rbind(cbind(1:14,rep(1,14),0:13,risk1[1:14],a1[1:14]),
         cbind(15:29,rep(0,15),0:14,risk2,a2))
  print(a)  # Table 11.2

# report:
a<- rbind(cbind(0:14,rep(1,15),risk1,a1),
      cbind(0:14,rep(0,15),risk2,a2))
  a<- a[-15,]  # remove interval with none at risk

xdat<- data.frame(week = factor(a[,1]),treat= a[,2],
                 risk= a[,3], y=a[,4])

#options(contrasts=c('contr.treatment'))
xreg<- glm(y~ treat + week+ offset(log(risk)),data=xdat,
          family=poisson)

bsum<- summary(xreg)
print(round(bsum$coef[,1:2],3)) # ..........Table 11.3



