

x<- scan('planet.dat',skip=5,list('',0,0,0,0,0,0))
dist<- x[[2]]
diam<- x[[3]]
mass<- x[[4]]
rota<- x[[5]]
revo<- x[[6]]
moon<- x[[7]]


par(mfrow=c(1,1))

dist<- round(dist/dist[3]*10,1)
ord<- 0:8
theory<- c(4,4 + 3*2^ord)

plot(c((-1:2),4:8),dist,xlab='Order',xlim=c(-1,10),ylim=c(2,900),
      ylab='Distance',type='n',log='y')
  points(c((-1:2),4:8),dist,cex=.7)
  off<- 1.1+c(.3,.2,0,0,0,0,0,.5,0)
  text(c((-1:2),4:8)+off,dist,x[[1]],cex=.7)
  lines(c(-1:8),theory,lwd=.4)

ord<- c(0:3,5:9)
a<- lm(log(dist)~ poly(ord,4))
  lines(c((-1:2),4:8),exp(a$fit),lty=3)
  title("Planetary distances",cex=.7)

