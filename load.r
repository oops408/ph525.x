install.packages("UsingR")

library(UsingR)
data("father.son",package="UsingR")

mean(father.son$sheight)
mean(father.son$sheight[round(father.son$fheight)==71])

X = matrix(1:1000,100,10)
X[25,3]

first_column <- 1:10
y <- cbind(x1=first_column, x2=first_column*2, x3=first_column*3, x4=first_column*4, x5=first_column*5)
sum(y[7,])

seq(10,1,-2)

head(X %*% matrix(1,ncol(X)))

X <- matrix(c(3,4,-5,1,2,2,2,-1,1,-1,5,-5,5,0,0,1),4,4,byrow=T)
ans <- solve(X) %*% matrix(c(10,5,7,4),4,1)
ans[3]

a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
c <- a %*% b
c[3,2]

sum(a[3,] * b[,2])

X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c('a','a','b','b')
beta <- matrix(c(5,2),nrow =2, ncol=1)
X[1:2,] %*% beta

X[3:4,] %*% beta

Y = matrix(y,nrow=length(y), ncol = 1)
beta <- solve(crossprod(X)) %*% t(X) %*%Y
beta  <- A %*% Y
beta[[3]] * -2

set.seed(1)
gravity_list <- replicate(100000, {
  y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)
  Y = matrix(y,nrow=length(y), ncol = 1)
  betahat <- solve(crossprod(X)) %*% t(X) %*%Y
  third_beta <- betahat[[3]] * -2
  return(third_beta)
})
popsd(gravity_list)

y.hat<-fit$fitted.values
r<-y.s-y.hat
SSR<-t(r)%*%r

X <- cbind(rep(1,N), x.s)
Var.X<-solve(t(X)%*%X)
Var.X[1,1]

diagonal<-diag(Var.X)
sqrt(diagonal[2]* (SSR / 48))

