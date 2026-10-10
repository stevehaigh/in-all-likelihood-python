# Computing binomial normal marginal
# using 16-point Gaussian quadrature

xi<- c(.0950125098,.2816035507,.4580167776,.6178762444,
      .7554044083,.8656312023,.9445750230,.9894009439)
xi<- c(-xi[8:1],xi)
wi<- c(.1894506104,.1826034150,.1691565193,.1495959888,
      .1246289712,.0951585116,.0622535239,.0271524594)
wi<- c(wi[8:1],wi)

dbinorm<- function(x,n,p,sig){
  th <- log(p/(1-p))
  u<- xi*4*sig
   pu <- exp(th+u)/(1+exp(th+u))
   pu<- ifelse(pu<exp(-25),exp(-25),pu)
   pu<- ifelse(pu>1-exp(-25),1-exp(-25),pu)
   probu<- dbinom(x,n,pu)*dnorm(u,0,sig)
   4*sig*sum(wi*probu)
  }

# 32-point gaussian quadrature

dbinorm32<- function(x,n,p,sig){
xi<- c(.0483076656,.1444719615,.2392873622,.3318686022, 
         .4213512761,.5068999089,.5877157572,.6630442669, 
         .7321821187,.7944837959,.8493676137,.8963211557, 
         .9349060759,.9647622555,.9856115115,.9972638618)
  xi<- c(-xi[16:1],xi)
wi<- c(.0965400885,.0956387200,.0938443990,.0911738786,  
          .0876520930,.0833119242,.0781938957,.0723457941,  
          .0658222227,.0586840934,.0509980592,.0428358980,  
          .0342738629,.0253920653,.0162743947,.0070186100)
  wi<- c(wi[16:1],wi)
th <- log(p/(1-p))
  u<- xi*4*sig
pu <- exp(th+u)/(1+exp(th+u))
   pu<- ifelse(pu<exp(-25),exp(-25),pu)
   pu<- ifelse(pu>1-exp(-25),1-exp(-25),pu)
   probu<- dbinom(x,n,pu)*dnorm(u,0,sig)
   4*sig*sum(wi*probu)
  }

# example:
xprob<-NULL
for (i in 0:16){
  a<- dbinorm32(i,16,0.5,4)
  xprob<- c(xprob,a)
}
#plot(xprob,ylim=c(0,.25))


