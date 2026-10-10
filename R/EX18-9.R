# Example 18.9

# waiting times between eruptions of
#  the Old Faithful geyser in Yellowstone National Park
  x<- scan('geyser.dat')
  N<- length(x)
 
h<- hist(x,nclass=20,prob=T,main='',
        xlab='Waiting time (minutes)',ylab='Density',#inside=F,style='old',
        ylim=c(0,.05))
  a<- density(x,bw=2.2)
  lines(a)


