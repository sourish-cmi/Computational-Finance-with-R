# Chapter 16: Statistical Risk Analysis

```{r}
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
data("edhec")
Rt = edhec$`Convertible Arbitrage`
a = VaR(Rt, p = 0.95, method = "gaussian", invert=FALSE)
b = -mean(Rt)-sd(Rt)*qnorm(.05)
```

```{r}
RRt = as.matrix(edhec)
cc = colnames(edhec)
c  =  VaR(RRt, p = 0.95, method = "gaussian", invert=FALSE)
d = matrix(0,93,13)
for(i in 1:93){
  j = i+59
  d[i,] = VaR(RRt[i:j,], p = 0.95, method = "gaussian", invert=FALSE)
}
e = index(edhec)[60:152]
plot(e,d[,1],ylab=cc[1],xlab='date',pch=20)
```

```{r}
Geltner.returns = [R(t) - R(t-1)*acf(R(t-1))]/1-acf(R(t-1))
```

```{r}
install.packages("robustbase")
library(robustbase)
md = c("gaussian", "modified", "historical")
cn = c("none","boudt","geltner")
VaRk = matrix(0,3,3)
for(i in 1:3){
     for(j in 1:3){
       Rc = Return.clean(R, method = cn[j], alpha = .01)
       VaRk[i,j]<-VaR(Rc, p = 0.95, method = md[i], invert=FALSE)
     }
}
```

```{r}
install.packages("QRM")
library(QRM)
Rc = Return.clean(R, method = cn[2], alpha = .01)
hillPlot(-Rc)
out = fit.GPD(-Rc,nextremes=25)
r = RiskMeasures(out,.95)[2]
```

```{r}
sd(edhec[,c(1)])
MeanAbsoluteDeviation(edhec[,c(1)])
SystematicRisk(edhec[,c(1)], edhec[,5])
SpecificRisk(edhec[,c(1)], edhec[,5])
DownsideDeviation(edhec[,c(1)])
PainIndex(edhec[,c(1)])}
```

```{r}
SharpeRatio(edhec[,c(1)])[2:3]
TreynorRatio(edhec[,c(1)], edhec[,c(5)])
CalmarRatio(edhec[,c(1)])
SterlingRatio(edhec[,c(1)])
BurkeRatio(edhec[,c(1)])
```