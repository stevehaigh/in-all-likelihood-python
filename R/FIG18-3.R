# Figure 18.3

x<- scan('air.dat',skip=9)
  x<- matrix(x,byrow=T,ncol=7)
  y<- x[,1]
  x<- x[,3]
  ord<- order(x)
  x<- x[ord]
  y<- y[ord]
  n<- length(x)

a<- lm(log(y)~log(x))

ly<- log(y)
lx<- log(x)

# ................Nearest neighbour smoothing
npar<- 20 # number of output points
nk <- 5  # number of neighbors

par(mfrow=c(2,2))
for (nk in c(7,3)){
  plot(x,y,xlab='Number of factories',
         ylab='Sulphur dioxide',type='n',log='xy')
  points(x,y,cex=.6)
  lines(x,exp(a$fit),lty=2)

  xx<- seq(min(lx)-.0001,max(lx),len=npar)
  xx<- lx
  yout<-NULL
for (xout in xx){
  sx <- abs(lx-xout)
  ordsx<- order(sx)
  yout0<- mean(ly[ordsx[1:nk]])
  yout<- c(yout,yout0)
}
  lines(exp(xx),exp(yout))
  title(paste('Number of neighbours = ',nk),cex=.6)
}

# .................... kernel smoothing

for (sd in c(0.2,0.05)){
  plot(x,y,xlab='Number of factories',
         ylab='Sulphur dioxide',type='n',log='xy')
  points(x,y,cex=.6)
  lines(x,exp(a$fit),lty=2)

  xx<- seq(min(lx)-.0001,max(lx),len=npar)
  xx<- lx
  yout<-NULL
for (xout in xx){
  sx <- (lx-xout)
  kx <- dnorm(lx,mean=xout,sd=sd)
  yout0<- sum(kx*ly)/sum(kx)         # kernel smoothing formula
  yout<- c(yout,yout0)
}
  lines(exp(xx),exp(yout))
  # comparison: using ksmooth()
  require(modreg)
  bw <- sd*qnorm(.75)/0.25  # convert to bandwidht
    ysmo<- ksmooth(lx,ly,kernel='normal',bandwidth=bw)
    lines(exp(ysmo$x),exp(ysmo$y),lty='dotted')  # ksmooth function
                         # expect some differences on the boundaries
                         # where different implementation may have
                         # different kernel on the boundaries
  title(paste('Standard deviation = ',sd),cex=.6)
}

