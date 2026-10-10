# Section 17.6: sleeping dog example:

x<- scan('dog.dat',skip=2)
  x<- matrix(x,byrow=T,ncol=4)
  xm <- apply(x,1,mean)
  ord<- order(xm)   # order of the means
  n<- nrow(x)

par(mfrow=c(2,2))
plot(rep(1:4,rep(n,4)),c(x),axes=F,
       type='n',xlab='Treatment',ylab='')
  points(rep(1:4,rep(n,4)),c(x),cex=.7)
  axis(side=1,at=c(1,2,3,4))
  axis(side=2)
  box()
  for (i in c(1,n/2,n)) {lines(1:4,x[ord[i],])}
  title(expression('(a) Sleeping dog data'))


y<- c(x)  # vectorized output
  N<- length(y)

# design matrix X: fixed effects
 X <- matrix(0,nrow=N,ncol=4)
 X[,1]<- rep(1,N)
 X[,2]<- rep(c(-1,1),c(2*n,2*n))/2         # effect of pressure with H
 X[,3]<- rep(c(1,-1,1,-1),rep(n,4))/2      # effect of high CO2
 X[,4]<- X[,2]*X[,3]
 XX<- t(X) %*% X

# operation with design matrix Z
Z1y <- function(y,n){     # Z'y for any y
        ymat<- matrix(y,ncol=4)
        ysum<- ymat %*% rep(1,4)
        return(c(ysum))
      }
Zb<- function(b,n){ # Zb for any b
      bb<- rep(b,4)
      return(bb)
     }

# initial value
  betahat <- solve(XX,t(X)%*%y)
  lambda<-0
  dog <- diag(rep(1,n))
  dog<- rbind(dog,dog,dog,dog)
  a<- lm(y~ -1+ dog +X[,2]*X[,3])

  s2<- var(a$res)
  s2b<- var(a$coef[1:n])
  lambda<- s2/s2b

i<-0
cat(i,round(c(s2,s2b,betahat,lambda),4),'\n')
olds2<- 100
olds2b<- 100
# update
while (abs(olds2-s2)/s2 + abs(olds2b-s2b)/s2b >.0001)
{
  i<- i+1
  olds2<- s2
  olds2b<- s2b  
  ybeta <- y - c(X %*% betahat)
  bhat <- Z1y(ybeta,n)/(4 + lambda)
  err<- ybeta - Zb(bhat)
    df <- n*sum(4/(4+lambda))
    s2<- sum(err^2)/(N-df)
  s2b <- 1/n*(sum(bhat^2) + s2*n*sum(1/(4+lambda)))
  lambda<- s2/s2b
  yb<-  y - Zb(bhat)
  betahat<- solve(XX,t(X)%*%yb)
cat(i,round(c(s2,s2b,betahat,lambda),4),'\n')
}  

b<- qqnorm(bhat,plot=F)
  plot(b,xlab='Normal quantiles',ylab='Dog effects',type='n')
  text(b$x,b$y,1:19,cex=.8)
  title(expression('(b) Estimated dog effects'))

breg<- lm(yb~ X[,2]*X[,3])
print(summary(breg))


## ........... for comparisons:
# if assume fixed dog effects:
  cat('Assuming fixed dog effects \n')
  print(summary(a)$coef[20:22,])

# testing interaction only:
int<- (x[,1]+x[,4]) - (x[,2]+x[,3])      # note: formula in page 452 is wrong
print(int)
cat('estimated interaction= ',mean(int),'(se = ',sd(int)/sqrt(19),')\n')

# hydrogen effect:
hydro <- (x[,3]+x[,4])/2 - (x[,1]+x[,2])/2
print(hydro)
cat('estimated hydrogen effect= ',mean(hydro),'(se = ',sd(hydro)/sqrt(19),')\n')

# CO2 effect:
CO2 <- (x[,2]+x[,4])/2 - (x[,1]+x[,3])/2
print(CO2)
cat('estimated CO2 effect= ',mean(CO2),'(se = ',sd(CO2)/sqrt(19),')\n')



