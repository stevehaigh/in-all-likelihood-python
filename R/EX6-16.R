# Example 6.16

xy <- scan('claim.dat',what=list(x=0,y=0))
  age<- xy$x
  claim<- xy$y
age0 <- age-mean(age)
n<- length(age)

# IWLS
X <- cbind(rep(1,n),age0)
bt <- c(log(mean(claim)),0)  # starting value
  cat('iteration= 0',', betahat =',bt,'\n')
for (i in 1:4){
   Xbt <- X %*% bt
   mu <- c(exp(Xbt))
   Y <- Xbt + (claim-mu)/mu
   S <- 1/mu        # !! this is a vector of variances,
                   # no need to turn it into a matrix
   XS <- X*S^(-1)
   XSX <- t(XS) %*% X
   XSY <- t(XS) %*% Y
   bt <- solve(XSX) %*% XSY
   cat('iteration= ',i,', betahat=',c(bt),'\n')
}

cat('Covariance matrix of betahat','\n')
  print(solve(XSX))
se2<- diag(solve(XSX))
  cat('standard errors of betahat',sqrt(se2),'\n')


# for comparison
xglm <- glm(claim~age0,family=poisson)
  cat('From GLM run:','\n')
  print(summary(xglm)$coef)
