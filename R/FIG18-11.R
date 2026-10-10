#  Figure 18.11

xy <- scan('surgery.dat',what=list(x=0,y=0))
  x<- xy$x
  y<- xy$y

# glm
x0 <- x-mean(x)
xglm <- glm(y~x,family=binomial)


lx<-x
npar<- 21
  xx<- seq(min(lx)-.0001,max(lx),len=npar)
  cutx<- cut(lx,breaks=xx)
  midx<- xx[-1] - (xx[2]-xx[1])/2

# bin statistics
n <- as.numeric(table(cutx))
ysum<- as.numeric(tapply(y,cutx,sum))
  ysum[is.na(ysum)]<- 0.

delta<- matrix(0,ncol=(npar-1),nrow=(npar-2))
for (i in 1:(npar-2)){
  delta[i,i]<- -1
  delta[i,i+1]<- 1
 }
#R1 <- t(delta) %*% delta
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


# ..........  starting values
  beta<- log(mean(y))- log((1-mean(y)))
  b<-0
  s2b<- .01
  lambda<- 1/s2b
  olds2<- 1000

par(mfrow=c(2,2))
plot(x,y,xlab='Age',ylab='Death rate',type='n',ylim=c(0,1))
  points(x,y,cex=.6)
  lines(x,xglm$fit,lty='dotted',lwd=1.5)

iter<-0
while(abs(olds2-s2b)/s2b >.0001){
 iter<- iter+1    
 olds2 <- s2b
  # step 1
    a<- Yfun(beta,b);  Y<- a$Y; wt<- a$wt
    b <- solve((diag(wt) + lambda*R1),wt*(Y-beta))
  # step 2
    beta<-  sum(wt*(Y - b))/sum(wt)
  # step 3
    smat <- solve(diag(wt) + lambda*R1)
    df <- sum(diag(smat)*wt)
    brb <- c(b%*%R1 %*%b) 
    mat <- smat * R1
    s2b <- 1/(npar-1-2)*(brb + sum(mat))   # sum(mat) = trace(smat %*% R1)
                                          #    since R1 is symmetric
    cat('iter= ', iter,'s2b= ',s2b,'df= ', df,'\n')
    lambda<- 1/s2b
} # end iter

  a<- Yfun(beta,b);
  lines(midx,a$p)
  title(expression(paste('(a) ',hat(sigma)[b]^2,'= 0.017')))

# prediction band
plot(x,y,xlab='Age',ylab='Death rate',type='n',ylim=c(0,1))
  points(x,y,cex=.6)
  lines(midx,a$p)
 
  eta<- a$eta+ 1.96* sqrt(diag(smat))
  upper<- exp(eta)/(1+ exp(eta))
    lines(midx,upper,lwd=.4)
  eta<- a$eta- 1.96* sqrt(diag(smat))
  lower<- exp(eta)/(1+ exp(eta))
    lines(midx,lower,lwd=.4)

  title(expression('(b) Prediction band'))
