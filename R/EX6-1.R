# Example 6.1

x<- matrix(scan('forbes.dat',skip=8),ncol=2,byrow=T)

temp<- (x[,1]-32)*5/9
alt<- -(287*293/9.8)*log(x[,2]*25.4/760)
print(c(287*293/9.8,25.4/760))

plot(temp,alt,xlab='Boiling point (Celsius)',
              ylab='Altitude (metres)',type='n')
  points(temp,alt,cex=.7)

a<- lm(alt~temp)
  lines(temp,a$fit,lty=3)

title(expression("Forbes' data"))


