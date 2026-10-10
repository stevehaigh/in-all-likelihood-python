
x<-8; n<-10

theta<- seq(0,1,len=100)
like <- dbinom(8,10,theta)
   like <- like/max(like)

plot(theta,like,type='l',xlab=expression(theta),
     ylab='Likelihood')


# also possible to use formula plotting, but
# we need to know the MLE in advance

#like<- function(theta){
#   dbinom(x,n,theta)/dbinom(x,n,x/n)
#  }
#plot(like,0,1,xlab=expression(theta),
#     ylab='Likelihood')

title(expression('Likelihood function'))


                         
