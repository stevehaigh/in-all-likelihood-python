# Example 12.1:

y <- c(10,22,15,23,17)
x1 <- c(1,-1,1,-1,1)
x2 <- c(1,1,0,0,-1)
x3 <- c(0,0,1,1,-1)

ylm<- lm(y~ x1 +x2+x3)
print(ylm$coef)

fn <- function(p){
  sum((y - p[1] - p[2]*x1 - p[3]*x2 - p[4]*x3)^2)
}
a<-nlm(fn,p= c(mean(y),0,0,0))
print(a$est)

# iterative solution:
y23 <- mean(y)
for (i in 1:25){
  yc <- c(y,y23)
  a <- apply(matrix(yc,nrow=2),1,mean) - mean(yc)
  b<-  apply(matrix(yc,nrow=2),2,mean) - mean(yc)
  y23 <- mean(yc) + a[2] + b[3]
  print(round(c(i,mean(yc),a[1],b[1],b[2]),3))
}

