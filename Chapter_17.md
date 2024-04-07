# Chapter 17: Supervised Learning

```{r}
library(MASS)
mod1 = lm(medv~rm,data = Boston) 
summary(mod1)
```

```{r}
Coefficients:
Estimate Std. Error t value Pr(>|t|)
(Intercept)  -34.671      2.650  -13.08   <2e-16 ***
rm             9.102      0.419   21.72   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1
Residual standard error: 6.616 on 504 degrees of freedom
Multiple R-squared:  0.4835,    Adjusted R-squared:  0.4825
```

```{r}
mod2 = lm(medv~.,data = Boston) 
summary(mod2)
```

```{r}
Call:
lm(formula = medv ~ ., data = Boston)

Residuals:
    Min      1Q  Median      3Q     Max 
-15.595  -2.730  -0.518   1.777  26.199 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
crim        -1.080e-01  3.286e-02  -3.287 0.001087 ** 
zn           4.642e-02  1.373e-02   3.382 0.000778 ***
indus        2.056e-02  6.150e-02   0.334 0.738288    
chas         2.687e+00  8.616e-01   3.118 0.001925 ** 
nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
age          6.922e-04  1.321e-02   0.052 0.958229    
dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
tax         -1.233e-02  3.760e-03  -3.280 0.001112 ** 
ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
black        9.312e-03  2.686e-03   3.467 0.000573 ***
lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.745 on 492 degrees of freedom
Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7338 
F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16
```

```{r}
### split train and test
n = nrow(Boston) ## total sample in the data
m = floor(n*0.8) ## training sample size 
set.seed(2021)
train_id = sample(1:n,size = m,replace = FALSE) 
train_id = sort(train_id) 
Boston_train=Boston[train_id,] 
Boston_test=Boston[-train_id,]
```


```{r}
mod3 = lm(medv~rm,data = Boston_train)
mod4 = lm(medv~.,data = Boston_train)
medv_hat3 = predict(mod3,newdata = Boston_test) 
medv_hat4 = predict(mod4,newdata = Boston_test)
medv_actual = Boston_test$medv
rmse3 = sqrt(mean((medv_hat3 - medv_actual)^2)) 
rmse4 = sqrt(mean((medv_hat4 - medv_actual)^2)) 
## OutSample Root Mean Square Error (RMSE) 
c(rmse3=rmse3,rmse4=rmse4)
```

```{r}
   rmse3    rmse4
7.129945 5.322710
```

```{r}
## In-sample RMSE
medv_bar3=mod3$fitted.values 
medv_bar4=mod4$fitted.values 
medv_actual_tr=Boston_train$medv
rmse3 = sqrt(mean(( medv_bar3- medv_actual_tr)^2)) 
rmse4 = sqrt(mean((medv_bar4 - medv_actual_tr)^2))
## In Sample Root Mean Square Error (RMSE) 
c(rmse3=rmse3,rmse4=rmse4)

```


### Logistic Regression

```{r}
set.seed(9879)
N = 200 ## number of points 
x = seq(-pi,pi,length.out=N) 
e = rnorm(N,mean = 0,sd=0.3) 
z = 0.01+0.45*x+ e
## z is the latent variable.
```


```{r}
y=z
y[z>0]=1
y[z<=0]=0
## y is observed response
D = cbind.data.frame(x,y) 
plot(x,y,pch=20,col='purple')
sigmoid_model = glm(y ~ x
                    ,family=binomial(link = "logit")
                    ,data=D)
summary(sigmoid_model)
y_hat = predict(sigmoid_model 
            ,data=data,type='response')
points(x,y)
points(x,y_hat,pch=20,col='red')
```

### Non-monotonic Relation with Logistic Regression

```{r}
## Beyond the sigmoid curve
set.seed(9879)
N=200
x = seq(-3.14,3.14,length.out=N) 
e = rnorm(N,mean = 0,sd=0.5)
z = sin(x)+e 

plot(x,z,pch=20,col='purple') 
y=z
y[z>0]=1
y[z<=0]=0 
plot(x,y,pch=20,col='purple')
data = cbind.data.frame(x,y)
```

```{r}
## sigmoid function
sigmoid_model = glm(y~x
                  ,family=binomial(link = "logit") 
                  ,data=data)

summary(sigmoid_model)
y_hat = predict(sigmoid_model,data=data,type='response')

points(x,y_hat,pch=20,col='red')
```


```{r}
## going beyond sigmoid function
beyond_sigmoid_model = glm(y~x+I(x^2)+I(x^3) 
                            ,family=binomial(link = "logit")
                            ,data=data)
summary(beyond_sigmoid_model)
y_hat2 = predict(beyond_sigmoid_model,data=data
                 ,type='response')
points(x,y_hat2,pch=20,col='blue')
```

### Discriminant Analysis

```{r}
data=read.csv(file = "default_of_credit_card_clients.csv",header = TRUE)

data$Default_colr<-rep(NA,nrow(data))
data$Default_colr[data$default_payment_next_month==1] <- "red"
data$Default_colr[data$default_payment_next_month==0] <- "lightgreen"

plot(log(data$LIMIT_BAL,base = 10),data$AGE
     ,col=data$Default_colr,pch=20
     ,xlab='AGE',ylab='Limint Balance in Log Scale')
```

```{r}
set.seed(038411) 
library(MASS) 
n=nrow(data)

## split the data into train and test
m = floor(n*0.7)
train_id = sort(sample(1:n,size=m,replace = FALSE)) 
train_data = data[train_id,]
test_data = data[-train_id,]

```

```{r}
## fitting LDA using MASS package
fit_lda = MASS::lda(default~AGE+log10(LIMIT_BAL) 
                           , data=train_data
                           , prior=c(0.7,0.3))

```

```{r}
pred = predict(fit_lda,newdata = test_data) 
test_data$pred_default = as.character(pred$class) 
test_data$pred_default = as.numeric(test_data$pred_default) 
```

```{r}
confusion_table = table(actual=test_data$default
                        ,predicted = test_data$pred_default)
accuracy=sum(diag(confusion_table))/sum(confusion_table) 
accuracy
> 0.7701
```

```{r}
### Fit QDA
fit_qda =MASS::qda(default~AGE+log10(LIMIT_BAL) 
                    ,data=data
                    ,prior=c(0.7,0.3))
pred = predict(fit_qda,newdata = test_data) 
test_data$pred_default = as.character(pred$class) 
test_data$pred_default = as.numeric(test_data$pred_default) 
confusion_table = table(actual=test_data$default
                        ,predicted = test_data$pred_default)
                        
accuracy=sum(diag(confusion_table))/sum(confusion_table) accuracy
> 0.7668
```

### Tree Structured Model

```{r}
## simulate data
set.seed(1)
N = 200
x = seq(-pi,pi,length.out=N)
z = sin(x)
y = z +rnorm(N,mean=0,sd=0.3)
D = cbind.data.frame(x=x,y=y) 
plot(NULL,xlim=c(-3.2,3.2),ylim=c(-1.5,1.5),xlab='x',ylab='y') 
points(D,col='aquamarine',pch=20)
## fit tree regression model and predict y
library(rpart)
tree_mod = rpart(y~x,data = D)
y_hat=predict(tree_mod,newdata = D) 
lines(x,y_hat,col='red',lwd=2)
```

```{r}
## fit random forest (Bootstrap on tree regression)
B=100
for(b in 1:B){
  id_star= sort(sample(1:N,N,replace = T)) 
  D_star = D[id_star,]
  tree_mod_b = rpart(y ~x,data = D_star) 
  y_hat_b=predict(tree_mod_b,newdata = D_star) 
  lines(x,y_hat_b,col='pink',lwd=2)
}
points(D,col='grey',pch=20) 
lines(x,y_hat,col='red',lwd=2) 
lines(x,z,lwd=2)
```