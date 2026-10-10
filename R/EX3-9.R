# Section 9-10
# Logistic model

xy <- scan('surgery.dat',what=list(x=0,y=0))
  x<- xy$x
  y<- xy$y
  n<- length(y)

# glm
x0 <- x-mean(x)
xglm <- glm(y~x0,family=binomial)
 
# Computing covariance matrix:
X<- cbind(rep(1,n),x0) # design matrix
p<- xglm$fit   # fitted probabilities
W<- p*(1-p)    # weight vector

XW<- X * W
  XWX<- t(XW) %*% X
  cat('The covariance matrix:','\n')
  print(solve(XWX))

b<- summary(xglm)
  cat('From GLM','\n')
  print(b$cov.scaled)  
