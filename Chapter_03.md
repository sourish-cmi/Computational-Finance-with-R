# Chapter 3: Solving Nonlinear Equations

## Find Implied Volatility 
```{R}
blackscholes = function(S, X, rf, T, sigma, CallPut=0){
  # S : Spot price
  # X : Strike price
  # rf: Risk free rate of interest
  # T : Time to maturity
  # sigma : volatility
  
  # CallPut : (default Call option) 
  #            0 for Call option  
  #            1 for Put option
  # 
  
  d1 = (log(S/X)+(rf-sigma^2/2)*T)/(sigma*sqrt(T))
  d2 = d1 - sigma * sqrt(T)
  if(CallPut == 0){
    value = max(S*pnorm(d1) - X*exp(-rf*T)*pnorm(d2), 0)    
  }else{
    value = max(X*exp(-rf*T) * pnorm(-d2) - S*pnorm(-d1), 0)
    
  }
return(value)
}
```
### Example 3.1.1

```{r}
# Current spot price
S=201.74 
# All available Strike prices
K=c(235, 210, 215, 245, 250, 225, 265, 220, 200, 205)
# Time to maturity
T=127/365
# Risk free rate of interest
r = 0.05
# Last traded call price 
C=c(3.15, 10.2, 8.35, 1.77, 1.3, 4.94, 0.57, 6.5, 15.3, 12.75)
# Vector for calculating the implied volatility
sig = rep(0,10)
for(i in 1:10){
sig[i] = implied_bisect(S,K[i],0,T,0,C[i],.0001,1)}
j = order(K)

plot(NULL,ylim=c(0.25,0.35)
     ,xlim=c(min(K),max(K))
     ,xlab="Strike"
     ,ylab="Implied Volatility")
grid(col='cadetblue4',lty=1)
points(K,sig,pch=20)
lines(K[j],sig[j],lwd=3,col='coral4')
```

## Bisection Algorithm
```{r}
implied_bisect = function(S, K, r, T, CallPut, C, tol, cond) {
# cond=1 if length of interval is used for tolerance, otherwise
# absolute value of the function at the midpoinr is used for tolerance.
if(CallPut==0 && C<S-K*exp(-r*T)|| CallPut!=0 && C<K*exp(-r*T)-S){
  guess=0  
}else{#finding the upper bound
 lower=0
 flower=blackscholes(S, K, r, T,lower,CallPut)- C
 upper=1
 fupper=blackscholes(S, K, r, T,upper,CallPut)- C
  while(flower*fupper>0){
  upper=upper*2
  fupper=blackscholes(S, K, r, T,upper,CallPut)- C}
#updating to bisected interval
 guess=0.5*(upper+lower)
 fguess=blackscholes(S, K, r, T,guess,CallPut)- C
  if (cond==1)
   while(upper-lower>tol){
    if(fguess*flower<0)
    upper=guess
    else
    lower=guess
    end
   flower=blackscholes(S, K, r, T,lower,CallPut)- C
   fupper=blackscholes(S, K, r, T,upper,CallPut)- C
   guess=0.5*(upper+lower)
   fguess=blackscholes(S, K, r, T,guess,CallPut)- C}
  else
   while(abs(fguess)>tol){
    if(fguess*flower<0)
    upper=guess
    else
    lower=guess
    end
   flower=blackscholes(S, K, r, T,lower,CallPut)- C
   fupper=blackscholes(S, K, r, T,upper,CallPut)- C
   guess=0.5*(upper+lower)
   fguess=blackscholes(S, K, r, T,guess,CallPut)- C}
  end
  
}
return(guess)
}
```


```{r}
guess=guess-call/vega
```

## R Functions for Root Finding

```{r}
polyroot(c(1, 2, 1))
```

```{r}
f = function(x)  2 * x + x^2-2
uniroot(f, c(-2, 2))
```
