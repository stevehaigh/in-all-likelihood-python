# Example 4.16
# population of EU countries (in millions)
y<-c(8,10,5,5,59,82,10,57,4,0.4,16,10,39,9,59,
    8,.7,10,1,10,4,2,0.4,38,22,5,2)
# y<- y+1    # much better normal transform

# Exercise 4.39: brain weight   # lhat=0.08
#y<- c(0.4,1.0 ,1.9 ,3.0 ,5.5 , 8.1,12.1, 25.6 ,50.0,56.0,
#  70.0, 115.0 , 115.0 , 119.5, 154.5,  157.0 , 175.0,  179.0,
#  180.0, 406.0, 419.0,  423.0,  440.0 , 655.0,  680.0, 1320.0 ,
#  4603.0, 5712.0)

n<- length(y)

par(mfrow=c(2,2))
a<-qqnorm(y,plot=F)
plot(a,xlab='Quantiles of standard normal',
       ylab='y',type='n')
points(a,cex=.6,lwd=.5)
title(expression('(a) Population data'))

ll<-NULL
nl<-40
lambda<-seq(.001,1.5,len=nl)
for (i in 1:nl){
  yl<- (y^lambda[i] - 1)/lambda[i]
  sl2<- var(yl)*(n-1)/n
  lli <- -n/2*log(sl2) + (lambda[i]-1)*sum(log(y))
  ll<- c(ll,lli)
}
ll<- ll-max(ll)

plot(lambda,ll,xlab=expression(lambda),
     ylab='Log-likelihood',
     type='n')
  lines(lambda,ll)
t  itle(expression(paste('(b) Profile likelihood of ',lambda)))
lmax<- max(lambda[ll==max(ll)])
  abline(v=lmax)
  cat('lambda-hat=', lmax,'\n')  # MLE of lambda


a<-qqnorm(sqrt(y),plot=F)
plot(a,xlab='Quantiles of standard normal',
       ylab=expression(paste('Quantiles of ',sqrt(y))),
       type='n')
points(a,cex=.6,lwd=.5)
title(expression('(c) Square-root transform'))

a<-qqnorm(log(y),plot=F)
plot(a,xlab='Quantiles of standard normal',
       ylab='Quantiles of log(y)',type='n')
points(a,cex=.6,lwd=.5)
title(expression('(d) Log-transform'))

