# 1. Preliminaries

### Example 1.1.2
```{R}
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

### Example 1.2.2
```{R}
x = c(-1,0,1)
f = c(1,2,-1)
h = c(0,0)
d=h
p=h
q=h
c = c(0,0,0)
for(i in 1:2){
  h[i] = x[i+1]-x[i]
  d[i] = (f[i+1]-f[i])/h[i]
}
u = 3*(d[2]-d[1])
c[2] = u/(h[2]+h[1])
for(i in 1:2){
  p[i] = f[i]/h[i]-c[i]*h[i]/6
  q[i] = f[i+1]/h[i]-c[i+1]*h[i]/6
}
```
