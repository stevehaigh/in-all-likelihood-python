
n<-5
x<- c(2.85,1.51,0.69,0.57,2.29)
theta<- seq(2.4,7,len=1000)

like<- ifelse(theta>max(x),theta^{-n},0)
  like<- like/max(like)

a<- theta[like>.05]
  print(range(a))

plot(theta,like,xlab=expression(theta),
     ylab='likelihood',type='l')
  abline(h=.05)
  title(expression('Uniform model'))

