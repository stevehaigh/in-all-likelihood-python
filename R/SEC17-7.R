# Section 17.7: growth curve analysis

x<- matrix(scan('ramus.dat'),byrow=T,ncol=5)
  x<- x[,2:5]
  xm <- apply(x,1,mean)
  ord<- order(xm)   # order of the means
  n<- nrow(x)
age<- seq(8,by=.5,len=4)

par(mfrow=c(2,2))
plot(rep(age,rep(n,4)),c(x),type='n',xlab='Age',ylab='Ramus height (mm)')
  points(rep(age,rep(n,4)),c(x),cex=.6)
  for (i in c(1,n/2,n))
     {    lines(age,x[ord[i],])}
  title(expression('(a) Data from 20 boys'))

# starting values:
  slope<-NULL
  evar<- NULL
  for (i in 1:n){
    a<- lsfit((age-mean(age)),x[i,])
    slope<- rbind(slope,a$coef)
    evar<- c(evar,var(a$res))
  }
  s0<- var(slope[,1])
  s1<- var(slope[,2])
  se<- mean(evar)

# ................................................... fixed effects
plot(slope[,1],slope[,2],xlab='Intercept',ylab='Slope',
     type='n')
  text(slope[,1],slope[,2],1:n,cex=.8)
  title(expression('(b) Separate estimation'))


# assume that random b0 and b1 are independent
#   which makes sense if age is centered.
 age<- age-mean(age)
 y<- c(t(x))  # vectorized data **** by individuals  ****
 ni<- rep(4,n)
 N<- length(y)

# fixed effects:
 X <- rep(1,N); X<- cbind(X,rep(age,n))
 XX<- t(X) %*% X

# ...........................   randoms effects operators
  Z0y<- function(y,n){
         a<- matrix(y,nrow=n,byrow=T)
         asum<- a %*% rep(1,ncol(a))
         return(c(asum))
       }
  Z0b<- function(b,ni){
         a<- rep(b,ni)
         return(a)
        }

  Z1y<- function(age,y,n){
         a<- matrix(y,nrow=n,byrow=T)
         asum<- a %*% age
         return(c(asum))
       }
  Z1b<- function(age,b,ni){
         a<- rep(age,n)
         bb<- rep(b,ni)
         return(a*bb)
        }
 # Z1'Z1 = diag[a<-i], where a<-i=sum(age^2)
   age2<- sum(age^2)

# .............................   iterations:
  beta<- solve(XX,(t(X)%*%y))
  b0<- slope[,1] - beta[1]
  rb0<- range(slope[,1])
  rb1<- range(slope[,2])

# iterate

cat(0,'s0=',s0,'s1=',s1,'se=',se,'\n')
for (i in 1:10){
# update b1
  yc<- y-X%*%beta - Z0b(b0,ni)
  lambda1<- se/s1
  b1<- Z1y(age,yc,n)/(age2+lambda1)

# update beta
  yc <- y-Z0b(b0,ni) - Z1b(age,b1,ni)
  beta<- solve(XX,(t(X) %*% yc))

# update b0
  yc<- y-X%*%beta - Z1b(age,b1,ni)
  lambda0<- se/s0
  b0<- Z0y(yc,n)/(ni+lambda0)

# update: variance components
  err<- y-X%*%beta - Z0b(b0,ni) - Z1b(age,b1,ni)
  df0 <- sum(ni/(ni+lambda0))
  df1 <- n*age2/(age2+lambda1)  
  se<- sum(err^2)/(N-df0-df1)
  s0 <- 1/n*(sum(b0^2) + se*sum(1/(ni+lambda0)))
  s1 <- 1/n*(sum(b1^2) + se*sum(1/(age2+lambda1)))

cat(i,'s0=',s0,'s1=',s1,'se=',se,'\n')
}
plot((beta[1]+b0),(beta[2]+b1),xlab='Intercept',ylab='Slope',
     type='n',xlim=rb0,ylim=rb1)
text((beta[1]+b0),(beta[2]+b1),1:n,cex=.8)
title(expression('(c) Random effects model'))

plot(slope[,2],(beta[2]+b1),xlab='Separate',
     ylab='Random effects',type='n')
points(slope[,2],(beta[2]+b1),cex=.8)
abline(0,1,lty=2)
title(expression('(d) Slope estimates'))



# standard error for the fixed effects:X S^{-1}(X-Z bx)
Xerr<-NULL 
for (i in 1:ncol(X)){
  yc<- X[,i]
  b0<- Z0y(yc,n)/(ni+lambda0)
  b1<- Z1y(age,yc,n)/(age2+lambda1)
  xerr<- yc- Z0b(b0,ni) - Z1b(age,b1,ni)
  Xerr<- cbind(Xerr,xerr)
}
XVX <- t(X) %*%Xerr/se
varmat<- solve(XVX)
cat('betahat=',round(beta,4),'\n')
cat('std-err=',round(sqrt(diag(varmat)),4),'\n')
cat('\n')

# ........................   ignoring repeated measures:
reg<- lm(y~X[,2])
cat('.... ignoring repeated measures:\n')
print(summary(reg)$coef)
cat('\n')

# ........................   comparison: two-staged LS
beta2<- solve(XX,(t(X)%*%y))
err0<- y- X%*%beta2
  err0<- matrix(err0,ncol=4,byrow=T)
V<- var(err0)
XVX <- n*t(X[1:4,]) %*% solve(V) %*% X[1:4,]
XVy <- 0; for (i in 1:n){
           XVy <- XVy+ t(X[1:4,]) %*% solve(V) %*% x[i,]
         }
beta2<- solve(XVX,XVy)
var2<- solve(XVX)
cat('.... two-staged LS\n')
cat('betahat',round(beta2,4),'\n')
cat('std err=',round(sqrt(diag(var2)),4),'\n')
cat('\n')

cat('covariance matrix with no assumption=','\n')
  print(round(V,4))

# covariance matrix of the repeated measures: using random effects:
Vran<- diag(se,4) + s0 + s1*outer(age,age,'*')
  cat('covariance matrix using random effects=','\n')
  print(round(Vran,4))
