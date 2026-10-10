# Example 6.18a: to produce Figure 6.8 and the first table 
# in page 180

#Y	SO2 content of air in micrograms per cubic metre
#X1	Average annual temperature in oF
#X2	Number of manufacturing enterprises employing 20 or more workers
#X3	Population size (1970 census); in thousands
#X4	Average annual wind speed in miles per hour
#X5	Average annual precipitation in inches
#X6	Average annual of days with precipitation per year

x<- scan('air.dat',skip=9)
  x<- matrix(x,byrow=T,ncol=7)
  y<- x[,1]
  x<- x[,3]
  ord<- order(x)
  x<- x[ord]
  y<- y[ord]
  n<- length(x)

par(mfrow=c(2,2))
plot(x,y,xlab='Industries',
         ylab='Sulphur dioxide',type='n')
  points(x,y,cex=.6)
  title(expression('(a) Pollution data'))

plot(x,y,xlab='Industries',log='x',
         ylab='Sulphur dioxide',type='n')
  points(x,y,cex=.6)
  title(expression('(b) Transform x-axis'))

lx<- log(x)
  lx2<- lx^2
xreg<- lm(y~lx+lx2)
  lines(x,xreg$fit,lty=2)
  asum<- summary(xreg)
  print(asum)

  a1<- qqnorm(xreg$res,plot=F)
