# Example 16.2                         
# Poisson data:

x<-c(3, 2, 5, 0 ,4)
  n<- length(x)
  mu<- seq(.1,10,length=100)
  ll<- -n*mu + sum(x)*log(mu)
  like<- exp(ll-max(ll))

# Prediction
y<- seq(1,10)
  ly<- log(y)
  y<- c(0,y)
  sumly<- c(0,cumsum(ly)) # ======= add 0!=1 for y=0 ==> log y!=0
  thetay<- (y+sum(x))/(n+1)
ll <- -(n+1)*thetay + (y+sum(x))*log(thetay) - sumly
  ll<- exp(ll-max(ll))
  like<- ll/sum(ll)

# plug-in
gy <- dpois(y,mean(x))

plot(y,like,xlab='y',
  ylab='likelihood',type='b',ylim=range(c(like,gy)))
  #lines(y,gy,lty='dotted',lwd=1.5)
  title('Predictive likelihood',cex=.7)

print(round(100-cumsum(like)*100,1))
