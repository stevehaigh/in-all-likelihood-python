# Example 5.5

x<- c(109,  88,  96,  96, 109, 116, 114,  96,
     85, 100, 113, 117, 107, 104, 101,  81)
y<- c(116, 77,  95,  79, 113, 122, 109,  94,
     91,  88, 115, 119, 100, 115,  95,  90)
n<- length(x)
.Random.seed<- c(0,1900,90,1)

par(mfrow=c(2,2))
plot(x,y,xlab='Verbal',ylab='Mathematical',type='n')
points(x,y,cex=.4)
title(expression('(a) IQ data'))

M1<- NULL
nb<-  500
for (i in 1:nb){
  id <- sample(1:n,replace=T)
    x1 <- x[id]; y1<- y[id]
    M1 <- c(M1,cor(x1,y1))
}

hist(M1,
     nclass=20,
     xlab=expression(paste(hat(rho),'*')),
     main='')
title(expression('(b) Bootstrap distribution'))

