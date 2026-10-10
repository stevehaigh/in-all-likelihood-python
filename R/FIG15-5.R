#  Figure 15.5

# profile likelihood for correlation coef:
pll<- function(p){
    mux <- p[1]
    muy <- p[2]
    sigx<- p[3]
    sigy<- p[4]
    c1<- sigx*sigy*(sqrt(1-rho^2))
    (n-2)*log(c1) + 1/2/(1-rho^2)*(sum((x-mux)^2)/sigx^2 -
             2*rho*sum((x-mux)*(y-muy))/sigx/sigy +
             sum((y-muy)^2)/sigy^2 )
    }

lrho<- function(rho,x)  # -2*loglikelihood
  {
 x1<- scale(x[,1]); x2<- scale(x[,2])
 x1<- x1*sqrt(n/(n-1)); x2<- x2*sqrt(n/(n-1));
 sy2<- sum(x2^2)
 sx2<-  sum(x1^2)
 sxy<- sum(x1*x2)
 ll<- n*log(1-rho^2) + (sy2-2*rho*sxy+ rho^2*sx2)/(1-rho^2)
 ll
  }


xdat<- matrix(scan('law.dat'),ncol=2,byrow=T)
  x<- (xdat[,1]); y<- (xdat[,2])
  n<- length(x)
  rho.obs<- cor(x,y)


# computing profile likelihood using nlm()
nr<- 40
rhopar<- seq(rho.obs/2,.95,len=nr)  
idone<-0
if (idone<1){
  ll0<- rep(0,nr)
  for (i in 1:nr){
    rho <- rhopar[i]
    ll0[i] <- -nlm(pll,c(mean(x),mean(y),sqrt(var(x)),sqrt(var(y))))$min
  }
  lik12<- exp(ll0-max(ll0))
}

plot(rhopar,lik12,xlab=expression(rho),
     ylab='likelihood',type='n')
   lines(rhopar,lik12,lwd=.3)

set.seed(1)
nb<- 4000

x<- xdat[,1]; y<- xdat[,2]
  mx<- mean(x); my<- mean(y); 
  sx<- sqrt(var(x)*(n-1)/n);  
  sy<- sqrt(var(y)*(n-1)/n);

for (ipar in 1:2){  # 1=parametric, 2= nonparametric
if (ipar==1){
  xs <- rnorm(n*nb)*sx + mx
  ys<- my+ rho.obs*sy/sx*(xs-mx) + sy*sqrt(1-rho.obs^2)*rnorm(n*nb)
}
if (ipar==2){
  id<- sample((1:n),n*nb,repl=T)
  xs<- x[id]; ys<- y[id]
}
xs<- matrix(xs,ncol=n)
  ys<- matrix(ys,ncol=n)
  xm<- c(xs%*% rep(1,n))/n
  ym<- c(ys%*% rep(1,n))/n

covxy<- ((xs-xm)*(ys-ym))%*% rep(1,n)
x2<- (xs-xm)^2 %*% rep(1,n); y2<- (ys-ym)^2%*% rep(1,n)
cor.b<- c(covxy/sqrt(x2*y2))   # bootstrap replications of cor

# bootstrap likelihood
rhoboot<- seq((min(cor.b)+.05),(max(cor.b)-.02),len=nr)

phi<- rep(0,nr)
for (i in 1:nr){
  phi[i]<- qnorm(sum(cor.b<rhoboot[i])/nb)
}
phi0<- qnorm(sum(cor.b<rho.obs)/nb)

# computing jacknife estimate of constant a
# for correlation coef
  cori<- rep(0,n)
  for (i in 1:n){
    cori[i]<- cor(x[-i],y[-i])
  } 
  mc<- mean(cori)
  a<- sum((mc-cori)^3)/6/sum((mc-cori)^2)^(3/2)

s <- 1+ a*(phi-phi0)
pmean<- -phi0*s + phi
ln<- dnorm(phi0,mean=pmean,sd=s)
lt<- ln/max(ln)

  if (ipar==1) lines(rhoboot,lt,lty='dashed',lwd=.7)
  if (ipar==2) lines(rhoboot,lt,lty='dotted',lwd=1.52)
} # end ipar

  abline(v= rho.obs)
  abline(h=exp(-1.96^2/2),lwd=.3)
  text(.7,.19,'95%',cex=.7)
  print(c(phi0,a))


