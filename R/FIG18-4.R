# Figure 18.4

set.seed(17)

n<- 128
sd<- 1
x<- rnorm(n)*sd

par(mfrow=c(2,2))
plot(cumsum(x),xlab='Index',ylab='b',type='l')
  title(expression('First-order random walk'))

x<- rnorm(n)*sd
plot(cumsum(x),xlab='Index',ylab='b',type='l')
  title(expression('First-order random walk'))


x<- rnorm(n)*sd
plot(cumsum(cumsum(x)),xlab='Index',ylab='b',type='l')
  title(expression('Second-order random walk'))


x<- rnorm(n)*sd
plot(cumsum(cumsum(x)),xlab='Index',ylab='b',type='l')
  title(expression('Second-order random walk'))

