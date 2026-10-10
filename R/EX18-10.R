#Example 18.10

# waiting times between eruptions of
#  the Old Faithful geyser in Yellowstone National Park
  x<- scan('d:/classes/statcomp/geyser.dat')
  N<- length(x)

npar<- 41
  xx<- seq(min(x)-.0001,max(x),len=npar)
  cutx<- cut(x,breaks=xx)
  midx<- xx[-1] - (xx[2]-xx[1])/2

# bin statistics
cat(round(midx,1),'\n')
cat(table(cutx),'\n')
y <-  as.numeric(table(cutx))

delta<- matrix(0,ncol=(npar-1),nrow=(npar-2))
for (i in 1:(npar-2)){
  delta[i,i]<- -1
  delta[i,i+1]<- 1
 }
# R1 <- t(delta) %*% delta   #  d=1
delta2<- delta[-1,-1]%*% delta
R1 <- t(delta2) %*% delta2

Yfun<- function(beta,b){
  eta <- beta + b 
  mu<- exp(eta)
  err<- (y-mu)/(mu+.000001)
  Y <- eta + err
  wt <- mu
  return(Y=c(Y),wt=c(wt),eta=c(eta),mu=c(mu))
}

# starting values
s2b<- 0.05
beta<- log(mean(y))
b<- rep(0,(npar-1))
olds2b<- 1000

iter<- 0
while(abs(olds2b-s2b)/s2b>0.001){
    lambda<- 1/s2b
	iter<- iter+1
    olds2b<- s2b
  # step 1
    a<- Yfun(beta,b);  Y<- a$Y; wt<- a$wt
    b <- solve((diag(wt) + lambda*R1),wt*(Y-beta))
    smat <- solve(diag(wt) + lambda*R1)
    df <- sum(diag(smat)*wt)
  # step 2
    beta<-  sum(wt*(Y - b))/sum(wt)
  # step 3
    smat <- solve(diag(wt) + lambda*R1)
    df <- sum(diag(smat)*wt)
    brb <- c(b%*%R1 %*%b) 
    mat <- smat * R1
    s2b <- 1/(npar-1-2)*(brb + sum(mat))   # sum(mat) = trace(smat %*% R1)
                                          #    since R1 is symmetric
    cat('iter= ',iter,'s2b= ',s2b,'df= ', df,'\n')

  } # end iter

  a<- Yfun(beta,b);
  #lines(midx,a$mu)
  #title(paste('d=2,','sigma<-b**2=',1/lambda),cex=.6)

# plotting density:
y0<- y/N/(midx[2]-midx[1])
plot(midx,y0,type='n',xlab='Waiting time (minutes)',ylab='Density')
   points(midx,y0,cex=.6)
   fhat <- a$mu/N/(midx[2]-midx[1])
   lines(midx,fhat)
   title(expression(paste(hat(sigma)[b]^2,'= 0.042')))

# histogram estimate: same degrees of freedom
  xx<- seq(min(x)-.0001,max(x),len=round(df))
    xhist<- hist(x,breaks=xx,plot=F,prob=T)
    #par(new=T); barplot(height=xhist$count,width=xhist$breaks,
    #                   hist=T,xlab='',ylab='',axes=F,
    #                   style='old',inside=F,lty=2)
 
# kernel density estimate: 
  a <- density(x,bw=2.2)
  lines(a,lty='dotted',lwd=1.5)
