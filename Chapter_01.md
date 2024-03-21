# 1. Preliminaries

### Example 1.1.2
```{r}
f<-function(x) 3*x^2-16*x+5
a<-0
b<-1
e<-0.005
while(b-a>e){
  c<-(a+b)/2
  if(f(a)*f(c)<0){b<-c}else{a<-c}}
sol=b
sol

## [1] 0.3359375
```
