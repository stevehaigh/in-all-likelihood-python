n<- 5
xn<- 3.5

# minus log-likelihood function 
llike <- function(theta){
  -log(dnorm(xn,mean=theta)) - (n-1)*log(pnorm(xn,mean=theta))
}

# nonlinear optimization, starting with 3.5
# the estimate is given by out$est
# the Fisher information is given by the Hessian term: out$hess
out<- nlm(llike,3.5,hess=T)
print(out)
