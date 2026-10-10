# Example 11.3

x<- matrix(scan('ulcer.dat'),ncol=12,byrow=T)
  x1<- x[,2:6];
  x2<- x[1:29,8:12]

#Change from begin --> end
a1 <- (x1[,1]-x1[,5])>0   # true=better
a2 <- (x2[,1]-x2[,5])>0   
table(a1)
table(a2)

# pairing the observations
X<- cbind(c(x1[,1:4]),c(x1[,2:5]))
  a1<- table(X[,1],X[,2])
  cat('Other +1 =',sum(a1[2,3]+a1[3,4]+a1[4,5]+a1[5,6]),'\n')
  cat('Other 0 =',sum(a1[2,2]+a1[3,3]+a1[4,4]+a1[5,5]),'\n')
  cat('Other -1 =',sum(a1[2,1]+a1[3,2]+a1[4,3]+a1[5,4]),'\n')

X<- cbind(c(x2[,1:4]),c(x2[,2:5]))
  a1<- table(X[,1],X[,2])
  cat('Other +1 =',sum(a1[2,3]+a1[3,4]+a1[4,5]+a1[5,6]),'\n')
  cat('Other 0 =',sum(a1[2,2]+a1[3,3]+a1[4,4]+a1[5,5]),'\n')
  cat('Other -1 =',sum(a1[2,1]+a1[3,2]+a1[4,3]+a1[5,4]),'\n')

