# Chapter 8: Monte Carlo Methods

### Linear Congruential Generator

```{r}
m = 17
a = 6
x = 7
u = rep(0,20)
for (i in 1:20){
  x=(a*x)%%m
  u[i]=x/m
}
```
### Example 8.2.1

```{r}
# Number of random samples
N=1000
# Simulate from uniforms
u=runif(N)
# transforms of uniforms
x= -log(1-u)
# exponential in R
y=rexp(N)
par(mfrow=c(1,2)) # plot parameters 
hist(x,freq=F,main="Exp from Uniforms",col="orange") 
hist(y,freq=F,main="Exp from Built-in R",col="orange")
```
