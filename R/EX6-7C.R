# Example 6.7 continued
# for the table in page 166.

xdat<- matrix(scan('compete.dat',skip=1),ncol=4,byrow=T)
  y<-  xdat[,3]
  den<- xdat[,2]
  n<- length(y)

xglm<- glm(y~ x0, family=Gamma)  # gamma fit produces
                                # the same param est. with exponential fit
                                # but different std.err.
b<- summary(xglm)$coef
print(b)
