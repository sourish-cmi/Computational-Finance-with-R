# Chapter 15: Resampling

```{r}
x = c(-0.76,1.64,-0.64,-1.26,-0.61,-1.79,3.42,0.55,-0.59,0.94)
library(timeDate)
n = length(x)   #10
s = skewness(x)  #0.801
sj = rep(0,n)
for(i in 1:10){
  y = x[-i]
  sj[i] = s+(n-1)*(s-skewness(y))
}  
#Generating the pseudo observations
bias = s-mean(sj) 
bias

## [1] -0.543


corrected = mean(sj)  
corrected

## [1] 1.344

variance = var(sj)/n  
variance

## [1] 0.292
```

```{r}
B = 1000
sb = rep(0,B)
set.seed(1)
for(i in 1:B){
  y = sample(x,n,replace=T)
  sb[i] = skewness(y)
}
hist(sb,breaks=20)
abline(v=0.8, col="blue")
mean(sb)  

## [1] 0.6024635

quantile(sb,0.025)  

## -0.2125221

quantile(sb,0.975)  

## 1.508589
```

```{r}
mu = mean(x)
sig = sd(x)
set.seed(1)
for(i in 1:B){
  y = rnorm(n,mu,sig)
  sb[i] = skewness(y)
}
hist(sb,breaks=20)
abline(v=0.8, col="blue")
quantile(sb,0.025) 

## -1.043546

quantile(sb,0.975) 

## 0.9517566
```

### Example 15.2.1

```{r}
x = stock_treasury[,c(3,5,7,9)]
x = as.ts(x)
xl = exp(diff(log(x),1))
weight = c(0.1,0.2,0.3,0.4)
for(i in 2:250){
  wealth = weight%*%(1+xl[i-1,])
  for(j in 1:4){
    weight[j]<-weight[j]*(1+xl[i-1,j])/wealth
  }
}
wealth    #1.984
set.seed(1)
for(k in 1:B){
  y = sample(seq(1:249),replace=T)
  xlb = xl[y,]
  weight = c(0.1,0.2,0.3,0.4)
  for(i in 2:250){
    wealth = weight%*%(1+xlb[i-1,])
    for(j in 1:4){
      weight[j] = weight[j]*(1+xlb[i-1,j])/wealth
    }
  }
  t[k] = wealth
}
hist(t)
```