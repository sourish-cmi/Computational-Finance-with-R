# Chapter 2: Vectors and Matrices

```{R}
#define a matrix for this example
set.seed(1010)
M = matrix(data = rnorm(12), ncol = 3)
round(M,2)
#run the function qr()
qr(M)$rank
```

### Example 2.1.1

```{R}
u = matrix(c(-2,1,-1,0,-1,3, -1,6, 0,2,1,-5), nrow=4)
qr(u)$rank
b = c(1,2,3,4)
ub = cbind(u,b)
qr(ub)$rank
```

```{R}
solve(A, b)
```

```{R}
b = c(3,3,2,1)
ub = cbind(u,b)
qr(ub)$rank
qr.solve(u,b)

##[1]-2 1 1
```

### Example 2.3.1

```{R}
x = A%*%u
u = x/sqrt(sum(x^2))
```

```{R}
ev = eigen(Sm)
evals = eigen(Sm, only.values = TRUE)$values
```
