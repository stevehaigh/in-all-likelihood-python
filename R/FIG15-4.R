# Figure 15.4
# getting empirical and BCa-bootstrap likelihood for the mean


source('meanlike.r')

# aircraft failure data:
x<- c(50,44,102,72,22,39,3,15,197,188,79,88,46,5,5,
     36,22,139,210,97,30,23,13,14)
n<- length(x)

v0<- mean(x)
# bootstrap step:
  nb<- 1000
.Random.seed<- c(0,1999,7,29)  # today's date
  xstar<- sample(x,n*nb,repl=T)
  xstar<- matrix(xstar,ncol=n)
  vstar<- c(xstar%*% rep(1,n)/n)

# bootstrap 
  nt <- 40
  phi0<- qnorm(sum(vstar<v0)/nb)
  th<- seq((min(vstar)+1),(max(vstar)-1),len=nt)
  phi<- rep(0,nt)

  for (i in 1:nt){
    phi[i]<- qnorm(sum(vstar<th[i])/nb)
  }

  a<- sum((x-mean(x))^3)/6/sum((x-mean(x))^2)^1.5
  s <- 1+ a*(phi-phi0)
  pmean<- -phi0*s + phi
  ln<- dnorm(phi0,mean=pmean,sd=s)    #.... BCa likelihood
  bcall <- log(ln) - max(log(ln))     


plot(th,bcall, xlab=expression(theta),
   ylab='Log-likelihood',type='n')
   lines(th,bcall,lwd=.3)
   abline(v=mean(x),lwd=.3)

empll<- meanlike(x)   # empirical likelihood
   lines(empll[[1]],empll[[2]],lty=2)





