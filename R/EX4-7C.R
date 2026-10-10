# Example 4.7 continued
# solving the Greenwood-Yule data using neq-binomial model:
# 
x<- c(447,132,42,21,3,2)
kk<- 0:4
n<- sum(x)


# Poisson fit
  lambda <- sum(x*c(0:5))/n; cat('lambda-hat = ',lambda,'\n')
  phat <- dpois(0:5,lambda)
  fhat.pois <- n*phat

# negative binomial fit:
dnbinom<- function(x,n,p){ 
         gamma(x+n)/gamma(n)/gamma(x+1)*p^n * (1-p)^x 
} 

fn<- function(p){
    r<- p[1]
    th<- p[2]
    ll <- -sum(x[1:5]*log(dnbinom(kk,r,th))) -x[6] * log(1-pnbinom(4,r,th))
    return(ll)
}

a<- nlm(fn,p=c(.5,.5),hes=T)
  est<- a$est
  cat('alpha-hat=',est[1],' pi-hat=',est[2],'\n')

# fitted frequencies:
phat<- c(dnbinom(kk,est[1],est[2]),1-pnbinom(4,est[1],est[2]))
  fhat<- n*phat

chi2<- sum((x-fhat)^2/fhat)
  cat('Chi2 goodness-of-fit = ',chi2,'\n')

print(cbind(Obs=x,'E-Pois'=round(fhat.pois,1),'E-NegBin'=round(fhat,1)))


