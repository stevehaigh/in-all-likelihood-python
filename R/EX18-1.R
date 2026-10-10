# Example 18.1

x<- scan('air.dat',skip=9)
  x<- matrix(x,byrow=T,ncol=7)
  y<- x[,1]
  x<- x[,3]
  ord<- order(x)
  x<- x[ord]
  y<- y[ord]
  n<- length(x)

par(mfrow=c(2,2))
plot(x,y,xlab='Number of factories',
         ylab='Sulphur dioxide',type='n',log='xy')
  points(x,y,cex=.6)
#title('Effect of industry on SO2 pollution')
a<- lm(log(y)~log(x))
  lines(x,exp(a$fit),lty='dashed')


# smoothed estimate
source('msmooth.r')   # contains smoothing function, 
                      # using mixed model approach in the book
  smo <- msmooth(log(x),log(y))
  lines(exp(smo$x),exp(smo$y))

# For comparison: .........  loess estimate
require(modreg)
b<- loess(log(y)~log(x))
  lines(x,exp(b$fit),lty='dotted')

