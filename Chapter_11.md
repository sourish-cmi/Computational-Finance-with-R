# Chapter 11: Variance Reduction

### Example 11.1.1

```{r}
r = 0.01
s = 0.3
s0 = 50
T = 1
u = rnorm(10000,0,1)
x = s0*exp(u*s*sqrt(T)+(r-s^2/2)*T)
f = function(z) max(z,0)
c = rep(0,8)
i = 0
for (K in seq(35,70,5)){
  i = i+1
  y = apply(matrix(K-x,1,10000),2,f)
  c[i] = cor(x,y)
}
```

### Example 11.3.1

```{r}
set.seed(7831)
sim.size = 50000
X = runif(sim.size,0,10)
Y = 10*exp(-2*abs(X-5))
cat('Estimate = ',round(mean(Y),3),'nn')

## Estimate =  0.999 nn

cat('Monte Carlo Error = ',round(var(Y),3),'nn')

## Monte Carlo Error = 3.993 nn

x = seq(0,10,0.1)
h = 10*exp(-2*abs(x-5))
plot(x,h,ylab="h(x)",type="l",lwd=2)
```

```{r}
w = function(x)dunif(x,0,10)/dnorm(x,mean=5,sd=1)
h = function(x)10*exp(-2*abs(x-5))
set.seed(7831)
sim.size = 50000
x = rnorm(sim.size,mean=5,sd=1)
y = h(x)*w(x)
cat('Estimate = ',round(mean(y),3),'nn')

## Estimate = 0.999 nn

cat('Monte Carlo Error = ',round(var(y),3),'nn')

## Monte Carlo Error = 0.358 nn

```

### Example 11.3.2

```{r}
set.seed(5472)
a = -5
x = rnorm(10000,mean=a,sd=2)
y = rep(0,length(x))
y[x<a] = 1
#print(length(y))
g = dnorm(x,mean=a,sd=2)
f = dnorm(x)
w = f/g
print(log(mean(w*y)))
```

### Example 11.4.1

```{r}

bar = rep(0,100)
hat = bar
l = 2
set.seed(1010)
for(i in 1:100){
  U = runif(1000,0,1)
  Y = -log(1-U)/l
  bar[i] = mean(Y)
  V = seq(0,.999,.001)
  V = V+U*.001
  Y1 = -log(1-V)/l
  hat[i] = mean(Y1)
}
mean(bar) 
## [1] 0.4979162
sd(bar) 
## [1] 0.01487852
mean(hat) 
## [1] 0.49998
sd(hat) 
## [1] 0.0004921935

```

### Stratifying Poisson Process

```{r}
SPP = function(Tfinal){
  u = rep(0,100)
  k = rpois(1,10)
  u = sort(runif(k))
  return(u)
}
PP = function(Tfinal){
  X = rep(0,100)
  Y = X
  T = 0
  i = 1
  while(T<Tfinal){
    i=i+1
    X[i]=rexp(1,10)
    Y[i]=Y[i-1]+X[i]
    T=T+X[i]}
  Z<-(Y[1:i-1])
  return(Z)
}
```