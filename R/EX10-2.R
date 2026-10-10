# Exampe 10.2: highly stratified binomial

y <- scan('binhigh.dat')
  n<- length(y)/2
strata <- rep(1:n,2)
  strata <- factor(strata)
treat<- c(rep(0,n),rep(1,n))

yreg<-glm(y ~ treat + strata, family='binomial')
  regsum<- summary(yreg)
  cat('constant and treatment estimates','\n')
  print(regsum$coef[1:2,])


# verifying the conditional analysis in Section 10.5, page 285:
y01 <- matrix(y,ncol=2)
  disc <- y01[abs(y01[,1]-y01[,2])>0.5,]   # discordant pairs
  p<- (sum(disc[,2])/nrow(disc))
  print(log(p/(1-p)))
a<-glm(disc[,2]~1,family='binomial')
  summary(a)





