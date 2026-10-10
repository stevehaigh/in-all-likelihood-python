# Example 6.15

x <- matrix(scan('trade.dat',skip=1),ncol=5,byrow=T)
  manag<- x[,1:2]
  city <- x[,3]
  domest<- x[,4]
x<- as.factor(x[,5])

xglm0<- glm(manag ~ domest*city, family=binomial)
  print(summary(xglm0))   # results maybe different from the text
                          # due to numerical problems

xglm1<- glm(manag ~ domest+city, family=binomial)
  cat('Deviance of additive model=',summary(xglm1)$deviance,'\n')


theta<-seq(-8,0,len=20)
dev<- NULL
for (a in theta){
  inter<- a*domest*city
  xglm1<- glm(manag ~ offset(inter)+domest+city, family=binomial)
  dev<- c(dev,xglm1$deviance)
}

plot(theta,dev,type='l',
     ylab='Deviance',
     xlab=expression(paste(beta[3],': interaction')))
  abline(h=3.84)
  title(expression('(a) Profile deviance'))

# ..........   conservative analysis: changing 0 to 1
manag[1,1]<- 1

dev<- NULL
for (a in theta)
{
inter<- a*domest*city
xglm1<- glm(manag ~ offset(inter)+domest+city, 
       family=binomial)
  dev<- c(dev,xglm1$deviance)
}

plot(theta,dev,type='l',
     ylab='Deviance',
     xlab=expression(paste(beta[3],': interaction')))
abline(h=3.84)

xglm0<-  glm(manag ~ domest*city, family=binomial)
  a<- summary(xglm0)

# normal approximation:
  nlike <- dnorm(theta,a$coef[4,1],a$coef[4,2])
  wilk<- 2*(max(log(nlike)) - log(nlike))
  lines(theta,wilk,lty='dotted',lwd=1.5)
  title(expression('(b) Conservative estimate'))


