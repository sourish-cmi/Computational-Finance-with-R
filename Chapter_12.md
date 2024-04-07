# Chapter 12: Statistical Methods

```{r}
install.packages("HSAUR3")
library(HSAUR3)
data("Forbes2000")
```

### Example 12.2.1

```{r}
x = Forbes2000
t = sort(table(x$Country), decreasing=TURE)
pie(t[1:10],las=2)
barplot(t[1:10],las=2)
```

```{r}
data("Forbes2000",package="HSAUR")
s = Forbes2000$sales
summary(s)
>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.010   2.018   4.365   9.697   9.547 256.330 
var(s)
> [1] 324.0933
sd(s)
[1] 18.00259
## range function gives two values, max and min
range(s)[2]-range(s)[1]
[1] 256.32
m=mean(s)
m3=sum((s-m)^3)/2000
# The skewness is positive and high
m4=sum((s-m)^4)/2000
m4/(var(s)^2)
> [1] 58.85951
```

```{r}
hist(log(Forbes2000$sales),freq=FALSE
     ,main = '',xlab='log(sales)'
     ,ylim = c(0,0.4),col='antiquewhite')
m = mean(log(Forbes2000$sales))
s = sd(log(Forbes2000$sales))
curve(dnorm(x,m,s),add=TRUE,col="chocolate",lwd=2)
lines(density(log(Forbes2000$sales)),lwd=2,col='blue')
```


### Example 12.5.1 : CEO compensation
```{r}
attach(ceo.compensation.2014)
x = log(Ratio)
library(timeDate)
skewness(x)  
# 0.08324829
kurtosis(x,method="moment") 
#3.873771
IQR(x)/sd(x)   
#1.201651
length(which(x<mean(x)+sd(x)&x>mean(x)-sd(x)))/length(x)  
#0.7211982
length(which(x<mean(x)+2*sd(x)&x>mean(x)-2*sd(x)))/length(x) 
#0.9562212
length(which(x<mean(x)+3*sd(x)&x>mean(x)-3*sd(x)))/length(x)  
#0.9907834
qqnorm(x)
abline(mean(x),sd(x))
```


```{r}

#The log returns of a stock are stored in the variable returns.
skewness(return) #gives the skewness
ks.test(return, pnorm, mean(return), sd(return) )
#conducts the Kolmogorov-Smirnov test of normality
# The following part is for testing heavy tailed-ness
t=rep(1,100)
for(i in 1:100) {t[i]=hillest(return, k=i)}
plot(t)
```