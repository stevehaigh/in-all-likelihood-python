# Figure 18.5


npar<- 21
for (d in c(1,2)){
delta<- matrix(0,ncol=(npar-1),nrow=(npar-2))
for (i in 1:(npar-2)){
  delta[i,i]<- -1
  delta[i,i+1]<- 1
 }

if (d <2) R1 <- t(delta) %*% delta
if (d>1){   # degree of differencing d=2
  delta2<- delta[-1,-1]%*% delta
  R1 <- t(delta2) %*% delta2
}

if (d==1) lambda<-3
if (d==2) lambda<-5

smat <- solve(diag((npar-1)) + lambda*R1)
 df <- sum(diag(smat))
 cat('df=  ',df,'\n')

par(mfrow=c(2,2))
 plot(seq(1:(npar-1)),smat[1,],ylim=c(-.05,.6),
       xlab='Index j',
       ylab='Weights',
       type='n')
 lines(seq(1:(npar-1)),smat[1,],lwd=.3)
 lines(seq(1:(npar-1)),smat[10,],lty=2)
 lines(seq(1:(npar-1)),smat[20,],lty=3,lwd=1.2)
 if (d==1) title(expression(paste('d=',1,', ',lambda,'= 3')))
 if (d==2) title(expression(paste('d=',2,', ',lambda,'= 5')))
 
} # -------------------------------------------------------- end differencing loop


