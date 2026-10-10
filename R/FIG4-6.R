#Section 4.7, Figure 4.6

# ..................... heart attacks
theta<- seq(0.2,.99,len=100)
x<-139
y<-239

ll <- x*log(theta/(theta+1)) - y*log(theta+1)
  like<- exp(ll-max(ll))

par(mfrow=c(2,2))
plot(range(theta),range(like),type='n',
     xlab=expression(theta),
     ylab='Likelihood')
  title(expression('(a) Heart attacks'))
  lines(theta,like,lwd=.4)
  abline(h=.15,lwd=.3)
  cat('95% CI=',li(theta,like,0.15),'\n')


# ..................... strokes
theta<- seq(0.7,1.99,len=100)
x<- 119
y<- 98

ll <- x*log(theta/(theta+1)) - y*log(theta+1)
  like <- exp(ll-max(ll))

plot(range(theta),range(like),type='n',
     xlab=expression(theta),
     ylab='Likelihood')
  title(expression('(b) Strokes'))
  lines(theta,like,lwd=.4)
  abline(h=.15,lwd=.3)
  abline(v=1,lwd=.3)
  cat('95% CI=',li(theta,like,0.15),'\n')

