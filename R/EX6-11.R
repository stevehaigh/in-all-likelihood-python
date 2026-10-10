# Example 6.11

xy <- scan('surgery.dat',what=list(x=0,y=0))
  x<- xy$x
  y<- xy$y

# glm
x0 <- x-mean(x)
xglm <- glm(y~x0,family=binomial)
  print(summary(xglm))
  cat('Null deviance=',xglm$null.deviance,', df=',xglm$df.null,'\n')
  cat('Deviance of Const+Age=',xglm$deviance,', df=',xglm$df.res,'\n')
