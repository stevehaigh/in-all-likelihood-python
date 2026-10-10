# Example 13.7

x<- c(-5.2,-1.9, -1.0, -0.7, -0.3, 0.0, 0.4 ,0.5, 2.3, 3.3)

# normal
l1<- sum(log(dnorm(x,0,1)))
cat('normal log-lik= ',l1,'\n')

# Cauchy:
l2<- sum(log(dcauchy(x,0,1)))
cat('Cauchy log-lik= ',l2,'\n')

