# Figure 9.7
# oneway random effects from Amstat aug 96: page 226

y<- matrix(scan('random.dat'),ncol=5,byrow=T)
  y<- c(y); 
  y<- 10*log10(y)
  x<- rep(c(1:5),rep(16,5))
  
par(mfrow=c(2,2))
plot(x,y,xlab='Person',ylab='10*log(concentration)',type='n')
  points(x,y,cex=.4)
  title(expression('(a) Estrone concentration'))

m<- 5
n<- 16
SST<- sum((y-mean(y))^2)  # 137.672 
xlm<- lm(y~ factor(x))    
  SSE<- sum(xlm$res^2)    #  24.40768  with df=75
  SSA<- SST- SSE

se <- SSE/(m*(n-1))       # 0.325
sa <- (SSA/m - se)/n      # 1.395

np<- 50
ssa <- seq(sa/4,sa*6,len=np)
sse <- seq(se/2,se*2,len=np)

fun1 <- function(sa,se){ # -2*log-likelihood
    m*((n-1)*log(se)+log(se+n*sa)) + SSE/se + SSA/(se+n*sa)
    }
loglik1 <- outer(ssa,sse,'fun1')
  loglik1 <- -loglik1/2
  loglik1 <- loglik1- max(loglik1)
  lik1 <- exp(loglik1)
  lik1 <- matrix(lik1,np,byrow=T)
contour(sse,ssa,lik1,level=seq(.1,1,by=.2),labcex=0.3,
        xlab=expression(sigma^2),
        ylab=expression(sigma[a]^2))
  title(expression('(b) Likelihood contour'))

# PRofile
likse <- apply(lik1,1,max)
  likse <- likse/max(likse)  # normalize to max=one
plot(sse,likse,type='l',xlab=expression(sigma^2), 
     ylab='Likelihood')
  title(expression(paste('(c) Likelihood of ',sigma^2)))

# normal approx
  sd <- .053
  nlike <- dnorm(sse,se,sd)
  nlike<- nlike/max(nlike)
  lines(sse,nlike,lty='dotted',lwd=1.5)
  abline(h=.15*max(likse),lwd=.3)
  print(range(sse[likse>.15*max(likse)]))

# Profile likelihood
liksa <- apply(lik1,2,max)
  liksa <- liksa/sum(liksa)/(ssa[2]-ssa[1])
  liksa <- liksa/max(liksa)
plot(ssa,liksa,type='l',
    xlab=expression(sigma[a]^2),
    ylab='Likelihood')
  title(expression(paste('(d) Likelihood of ',sigma[a]^2)))
  abline(h=.15*max(liksa),lwd=.3)
  print(range(ssa[liksa>.15*max(liksa)]))
  print(sum(liksa[liksa>.15*max(liksa)]*(ssa[2]-ssa[1])))

# normal approx
  sd <- .895
  nlike <- dnorm(ssa,sa,sd)
  nlike <- nlike/max(nlike)
  lines(ssa,nlike,lty='dotted',lwd=1.52)

# Fisher information and covariance matrix:
mse<- SSE/m/(n-1)
msa<- SSA/m
ii<- matrix(0,2,2)
  ii[1,1] <- m*(n-1)/2/mse^2 + m/2/msa^2
  ii[1,2]  <- m*n/2/msa^2
  ii[2,1] <- ii[1,2]
  ii[2,2] <- m*n^2/2/msa^2
print(ii)
print(solve(ii))
cat('Standard errors=',sqrt(diag(solve(ii))),'\n')


