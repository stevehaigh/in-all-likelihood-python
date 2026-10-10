# Example 15.4

source('meanlike.r')
# aircraft failure data
x<- c(50, 44, 102, 72, 22, 39, 3, 15,
    197, 188, 79, 88, 46, 5, 5, 36,
    22, 139, 210, 97, 30, 23, 13, 14)
  n<- length(x)
  tobs <- mean(x)
  x<- sort(x)

# parametric likelihood
th <- seq(30,105,len=40)
ll <- -n*log(th) - sum(x)/th
ll<- ll-max(ll)
plot(th , ll,xlab=expression(theta),
     ylab='Log-likelihood',type='n')
  lines(th, ll,lwd=.3 )

# empirical likelihood
b <- meanlike(x)
  lines(b[[1]], b[[2]], lty=2)
  abline(v = mean(x))


