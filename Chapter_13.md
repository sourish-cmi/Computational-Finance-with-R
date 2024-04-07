# Chapter 13: Inferential Statistics

### Example 13.0.1

```{r}
runningmean = function (x,N){
 y = sample(x,N, replace=TRUE)
 c = cumsum(y)
 n = 1:N
 c/n
}
set.seed(10101)
u = runningmean(c(0,1), 1000)
x=1:1000; plot(u~x, type="l",ylim=c(0,1));
replicate(10, lines(runningmean(c(0,1), 1000)~x,
type="l", col=rgb(runif(3),runif(3),runif(3))))
```

```{r}
x = rbinom(1000,n,p)/n; qqnorm(x); abline(p,sqrt(p*(1-p)/n))
```


```{r}
f1<-function(p) p^4
f2<-function(p) (1-p)^4
f3<-function(p) 6*p^2*(1-p)^2
curve(f1,xlim=c(0,1),xlab="p",ylab="L(p)",lwd=2)
curve(f2,add=TRUE,col="blue",lwd=2)
curve(f3,add=TRUE,col="red",lwd=2)
```

```{r}
#R code for finding the MLE:
# Code for gamma distribution MLE
# x stores the data
# a1, b1 store current estimates of alpha and beta
# Generate data from Gamma(alpha=2,beta=3) distribution)
set.seed(10101)
x=rgamma(20,2,3)
# Set low and high starting values for the searches
aL=.001;
aH=20;
bL=.001;
bH=20;
# Use method of moments for starting values
a1=mean(x)^2/(mean(x^2));
b1=mean(x)/(mean(x^2));
a1
b1
A=a1;
B=b1;
# Define the derivative of the log likelihood
deriva=function(a,b,x){
  n=length(x);
  n *log(b)+sum(log(x))-n*digamma(a);
}
derivb=function(a,b,x){
  n=length(x);
  n*a/b-sum(x);
}
dist=1;
cc=1;
toler=.0001;
while(dist>toler){
  a2=uniroot(deriva,c(aL,aH),b=b1,x)$root;
  b2=uniroot(derivb,c(bL,bH),a=a2,x)$root;
  dist=sqrt((a2-a1)^2+(b2-b1)^2);
  a1=a2;
  b1=b2;
  A=c(A,a1);
  B=c(B,b1);
  cc=cc+1
}
a1

## [1] 2.455892

b1

## [1] 3.46543

cc

## [1] 45
```

```{r}
a = rep(0,100)
b = a
c = 1.96/sqrt(500)
set.seed(10101)
for(i in 1:100){
  x = rexp(500,5) #mu=0.2, n=500
  a[i] = mean(x)-c*sd(x) #xbar-z s/sqrt(n)
  b[i] = mean(x)+c*sd(x)
}
length(which(a>.2|b<.2))/100
plot(a,ylim=c(.15,.25),type="n", ylab="Interval")
for(i in 1:100){lines(c(i,i),c(a[i],b[i]))}
abline(.2,0)
index<-which(a>.2|b<.2)
cbind(a[index],b[index])
```


```{r}
ciz = function(x, sigma, alpha=0.05){
  z = qnorm( 1-alpha/2)
  sdx = sigma/sqrt(length(x))
  c(mean(x) - z*sdx, mean(x) + z*sdx)
}
cit= function(x, alpha=0.05){
  n = length(x)
  t = qt( 1-alpha/2,n-1 )
  sdx = sd(x)/sqrt(n)
  c(mean(x) - t*sdx, mean(x) + t*sdx)
}
set.seed(10101)
x = rnorm(10)
ciz(x,1,0.05)    

## [1] -0.7085761 0.5310140

cit(x,0.05)      

## [1] -0.5982957 0.4207337
```