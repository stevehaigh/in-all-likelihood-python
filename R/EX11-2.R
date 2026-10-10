# Example 11.2
par(mfrow=c(2,2))

y<- scan('asthma.dat')
  n<- length(y)
  y12<- cbind(y[-n],y[-1])
  print(table(y12[,1],y12[,2]))

plot(y,xlab='Time',ylab=expression(x[t]),type='l')
  lines(y,lwd=.5)
  title(expression('(a) Asthma attacks'))

# ..........................    pollution data:

x <- matrix(scan("pollute.dat"), ncol=5,byrow=T)
  x<- x[,2]  # smoke concentration in ppm 
plot(x,xlab='Time',ylab=expression(z[t]),type='l')
  lines(x,lwd=.5)
  title(expression('(b) Pollution level'))

