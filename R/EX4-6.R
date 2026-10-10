# Example 4.6

k<- c(0,1,2,3,4,12)
nk<- c(38,26,8,2,1,1)

kfac<- function(x){
      n<- length(x)
      res<-NULL
      for (i in 1:n){
         ri<- ifelse(x[i]>1,prod(c(1:x[i])),1)
         res<- c(res,ri)
       }
      res
}

kk<- kfac(k)
  pk<- nk/sum(nk)
  yk <- log(pk*kk)

par(mfrow=c(2,2))
plot(k,yk,ylab=expression(paste('Log(',p[k], 'k!)')))
  title(expression('(a) Poisson plot'))


mu<- seq(.3,1.3,len=100)
ll<- -mu*sum(nk) + log(mu)* sum(k*nk)
  like<- exp(ll-max(ll))

mle<- sum(k*nk)/sum(nk)
  source('li.r')
  a<- li(mu,like,0.15)
  cat('mle =',mle,',  95% CI=',a,'\n')

plot(mu,like,xlab=expression(theta),
     ylab='Likelihood',type='l')

# .................     excluding the month with k=12

k<- c(0,1,2,3,4)
nk<- c(38,26,8,2,1)

ll<- -mu*sum(nk) + log(mu)* sum(k*nk)
  like<- exp(ll-max(ll))

   lines(mu,like,lty='dotted',lwd=1.5)
   abline(h=.15)

mle<- sum(k*nk)/sum(nk)
  source('li.r')
  a<- li(mu,like,0.15)
  cat('mle =',mle,',  95% CI=',a,'\n')
  cat('note: 0.53<theta<0.96 in the book is wrong. Should be 0.53<theta<0.90.','\n')

title(expression('(b) Likelihood functions'))


