# Example 10.9
par(mfrow=c(1,1))

# normal mean 

x<- c(-5.3, -4.5, -1.0, -0.7,  3.7,  3.9,  4.2,  5.5,  6.8,  7.4,  9.3)
n<- length(x)
  mx<- mean(x)
  sx <- sqrt(var(x))
  df <- n-1
  se <- sx/sqrt(n)
  alp<- 0.05
cat('95% t-interval',mx+qt(alp/2,df)*se,mx-qt(alp/2,df)*se, '\n')

th <- seq(mx-4*se,mx+4*se,len=40) 

source('li.r')
cut <- exp(-qchisq(1-alp,1)/2)
tstat <- sqrt(n)*(mx - th)/sx
  llnorm <- - n*(mx-th)^2/2/sx^2
      llnorm<- llnorm - max(llnorm)
print(li(th,exp(llnorm),cut))
  llp <- -n/2*log(1 + tstat^2/(n-1))      # PROFILE 
      llp<- llp- max(llp)
print(li(th,exp(llp),cut))
 llm <- -(n-2)/2*log(1 + tstat^2/(n-1))   # MODIFIED PROFILE
      llm<- llm- max(llm)
print(li(th,exp(llm),cut))


plot(th,llnorm,type='l',xlab=expression(mu),
     ylab='Log-likelihood')
  lines(th,llp,lty=2)
  lines(th,llm,lty='dotted',lwd=1.5)
  abline(h=-qchisq(1-alp,1)/2,lwd=.3)
