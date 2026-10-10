# empirical likelihood for the mean
meanlike <- function(x){
 n<- length(x)

f1<- function(lambda,x){
     x/(n-lambda*x)
    }

 sx<- sort(x)
 nt<- 40
 nlam<-40
 th<- seq(1.01*sx[1],(sx[n-1]+.99*(sx[n]-sx[n-1])),len=nt)
 theta<- NULL
 loglik<- NULL
 for (i in 1:nt){
   a<- x-th[i]
   lam <- seq(n/min(a),n/max(a),len=nlam)
   lam <- lam[c(-1,-nlam)]
   glam <-outer(lam,a,'f1')
   glam<- c(glam %*% rep(1,n))
   lam0<- approx(glam,lam,xout=0)$y
   if (is.na(lam0)==F) {   # lam0 exists
      w <- 1/(n-lam0*a)
      if (min(w)>0 & max(w)<1){  # proper distribution
        ll<- sum(log(w))
        theta<- c(theta,th[i])
        loglik <- c(loglik,ll)
      }
   } # endif
 }# endfor

 loglik<- loglik - max(loglik)
 out <- list(theta= theta, loglik=loglik)
 return(out)
} # end function
