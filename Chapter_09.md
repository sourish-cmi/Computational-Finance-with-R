# Chapter 9: Lattice Models

```{r}
## simulate from Binomial Asset Pricing Model
sim.BAPM = function(P0,n,r,sigma){
  # n is the number of periods of the chain.
  # P0 is the value of the asset at the initial time.
  # r is the risk free rate.
  # sigma is the volatility.
  P = rep(NA,n) # Define a vector for price path
  P[1] = P0 # Initial price
  u = exp(sigma/sqrt(n)) # Up-factor
  d = 1/u # Down-factor
  p = (1+(r/n)-d)/(u-d) # Risk-neutral probability
  q = 1-p
  for(i in 2:n){
      # Drawing random toss from an unbiased coin
      toss<-sample(c("Head","Tail"),1,replace = TRUE
      ,prob = c(p,q))
      # If head - price goes up by factor u
      if(toss=="Head")P[i]<-P[i-1]*u
      # If tail - price goes down by factor d
      if(toss=="Tail")P[i]<-P[i-1]*d
  }
  return(P)
}
set.seed(1234)
P = sim.BAPM(P0=100,n=5000,r=1,sigma=1)
plot(NULL,xlim=c(0,1),ylim=c(min(P),max(P))
     ,xlab="Time"
     ,ylab="Price")
grid(col='cadetblue4',lty=1)
points(seq(1:5000)/5000, P, type="l"
       ,lwd=2,col='brown',lty=1)

```


<p align = "center">
<img src="./Figure/fig9_4.jpg" alt="drawing" width="400" height="400"/>
</p>


```{r}
EuroPut = function(S, T , K, r, sigma , N){
  dT=T/N  #length of each subinterval
  u=exp(sigma*sqrt(dT)) #using the CRR model
  d=1/u
  p=(exp(r*dT)-d)/(u-d)
  tree=matrix(NA,nrow=(N+1),ncol=(N+1))  #setting up the matrix
  for(i in 0:N){
    tree[i+1,N+1]=max(0,(K-S*u^i*d^(N-i)))
    #The pay-off at terminal time with i upward movements
  }
  for(j in (N-1):0){
    for(i in 0:j){
    tree[i+1,j+1]=exp(-r*dT)*(p*tree[i+2,j+2]
    +(1-p)*tree[i+1,j+2])
    #The expected value of the option at each previous step
    }
  }
  price=tree[1,1]
  return(price)
}
EuroPut(S=10,T=10,K=11,r=0.05,sigma=0.1,N=10)

## [1] 0.09864932
```

```{r}
AmericanCall = function(T, S, K, r,sigma, N) {
  dT =  T / N
  u  =  exp(sigma * sqrt(dT))
  d  =  1/u
  p=(exp(r*dT)-d)/(u-d)
  tree=matrix(NA,nrow=(N+1),ncol=(N+1))
  for(i in 0:N){
    tree[i+1,N+1] = max(0,(S*u^i*d^(N-i)-K))
  }
  for(j in (N-1):0){
    for(i in 0:j){
      # binomial value
      tree[i+1,j+1] = exp(-r*dT)*(p*tree[i+2,j+2]+(1-p)*tree[i+1,j+2]) 
      # exercise value
      exercise = S*u^i*d^(j-i)-K 
      if(tree[i+1,j+1] < exercise) tree[i+1,j+1] = exercise  #***
    }
  }
  americanCall = tree[1,1]
  return(americanCall)
}
AmericanCall(S=12,T=10,K=12,r=0.05/250,sigma=0.01,N=100)

## [1] 0.1631424
```
