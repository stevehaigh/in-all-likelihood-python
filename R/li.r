# computing likelihood intervals at certain alpha levels
li<- function(th,like,alpha=0.15){
  that <- mean(th[like==max(like)])
  lowth <- th[th < that]  
  lowlik <- like[th < that] 
  if (length(lowth)<2) {lowval <- min(th) }
  if (length(lowth)>1)
      {  lowval <- approx(lowlik,lowth,xout=alpha)$y}

  upth <- th[th > that]
    if (length(upth)<2 ) {return(c(lowval,max(th))) }
   if (length(upth)> 1){
    uplik <- like[th > that]  
    upval <- approx(uplik,upth,xout=alpha)$y
    return(c(lowval,upval))
  }
  }
