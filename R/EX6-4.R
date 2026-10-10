# Example 6.4

x<- matrix(scan('seed.dat'),ncol=2,byrow=T)
y<-x[,1]  # success
n<-x[,2]  # number of planted seeds

seed<- c(rep(0,11),rep(1,10))
extract<- c(rep(0,5),rep(1,6),rep(0,5),rep(1,5))

reg<-glm(cbind(y,n-y)~ seed*extract,family=binomial)
  regsum<- summary(reg)
  print(regsum$coef)

# how X matrix is setup
N<- length(y)
X<- matrix(0,nrow=N,ncol=4)
  X[,1]<- 1
  X[,2]<- seed
  X[,3]<- extract
  X[,4]<- seed*extract
print(X)


