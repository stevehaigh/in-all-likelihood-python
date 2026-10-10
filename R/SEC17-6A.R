# Sec 17.6: one-way random effects:

# Fears et al data
x<- matrix(scan('random.dat'),ncol=5,byrow=T);
  x<- 10*log10(x)
  q<-5
  n<- 16

y<- c(x)              # data vector
Zb<- function(n,b){   # this returns Zb without having Z explicitly
    lb<- length(b)
    aa<- rep(b,rep(n,lb))
    return(aa)
}


# initial estimates
  xm <- apply(x,2,mean)
  xvar<- apply(x,2,var)
  s2<- mean(xvar)
  s2b<- var(xm)
  lam<- s2/s2b


#iterating to estimate all parameters:
for (i in 1:5){
  mu <- mean(y)
  b <- n*(xm - mu)/(n+lam)
  err <- y - mu - Zb(n,b)
  df <- q*n/(n+lam)
  s2 <- sum(err^2)/(q*n-df)
  s2b <- mean(b^2) + 1/(n/s2 + 1/s2b)
  lam <- s2/s2b
cat(i,round(c(mu,b,s2b,s2),4),'\n')
}


