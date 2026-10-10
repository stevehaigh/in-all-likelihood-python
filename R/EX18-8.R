# Example 18.8

xy <- scan('surgery.dat',what=list(x=0,y=0))
  x<- xy$x
  y<- xy$y

# parametric glm
x0 <- x-mean(x)
xglm <- glm(y~x,family=binomial)


lx<-x
npar<- 21
  xx<- seq(min(lx)-.0001,max(lx),len=npar)
  cutx<- cut(lx,breaks=xx)
  midx<- xx[-1] - (xx[2]-xx[1])/2

# bin statistics
cat(table(cutx),'\n')
n <- as.numeric(table(cutx))
cat(tapply(y,cutx,sum),'\n')

ysum<- as.numeric(tapply(y,cutx,sum))
  ysum[is.na(ysum)]<- 0.

delta<- matrix(0,ncol=(npar-1),nrow=(npar-2))
for (i in 1:(npar-2)){
  delta[i,i]<- -1
  delta[i,i+1]<- 1
 }
#R1 <- t(delta) %*% delta         # d=1
delta2<- delta[-1,-1]%*% delta
R1 <- t(delta2) %*% delta2

Yfun<- function(beta,b){
  eta <- beta + b 
  p<- exp(eta)/ (1+ exp(eta))
  err<- (ysum-n*p)/(n*p*(1-p)+.000001)
  Y <- eta + err
  wt <- n*p*(1-p)
  return(Y=c(Y),wt=c(wt),eta=c(eta),p=c(p))
}

# IWLS iterations
nrep<- 5  

# starting values
beta<- log(mean(y))- log((1-mean(y)))
b<-0

par(mfrow=c(2,2))
for (lambda in c(5,.5)) {
  plot(x,y,xlab='Age',ylab='Death rate',type='n',ylim=c(0,1))
  points(x,y,cex=.6)
  lines(x,xglm$fit,lty='dotted',lwd=1.5)

  for (irep in 1:nrep){
  # step 1
    a<- Yfun(beta,b);  Y<- a$Y; wt<- a$wt
    b <- solve((diag(wt) + lambda*R1),wt*(Y-beta))
    smat <- solve(diag(wt) + lambda*R1)
    df <- sum(diag(smat)*wt)
  # step 2
    beta<-  sum(wt*(Y - b))/sum(wt)
    cat('beta, df =',beta,df,'\n')
  } # end irep

  a<- Yfun(beta,b);
  lines(midx,a$p)
  if (lambda==5) title(expression(paste(sigma[b]^2,'= 0.2')))
  if (lambda==.5) title(expression(paste(sigma[b]^2,'= 2')))

}
