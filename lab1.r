#Create the vector
#1a
a <- c(1:20)

#1b
b

#1c
c <- c(1:20,19:1)

#1d
temp <- c(4,6,3)


#1e
e <- rep(temp,10)

#1f
f <- c(rep(temp, 10),4)

#1g
g <-c(rep(4,10), rep(6,20), rep(3, 30))

#4 calcuating the following
#4a
a <- c(10:100)
sum1 = sum(a^3) + sum(4*a^2)

#4b
b <- c(1:25)
sum2 <- sum((2^b)/b) + sum((2^b)/b + (3^b)/b)

#6
set.seed(50)
xVec <- sample(0:999, 250, replace = TRUE)
yVec <- sample(0:999, 250, replace = TRUE)

#6a
yVec1 <- yVec[-1]
xVec1 <- xVec[-250]
a <- yVec1 - xVec1

#6b
yVec1 <- yVec[-250]
xVec1 <- xVec[-1]
b <- sin(yVec1)/cos(xVec1)

#6c
xVec1 <- xVec[-249:-250]
xVec2 <- xVec[c(-1,-250)]
xVec3 <- xVec[-1:-2]
c <- xVec1 + xVec2 + xVec3

#6d
xVec4 <- xVec[-1]
xVec5 <- xVec[-250]
a = exp(xVec4)
b = xVec5 +10
sum3 = sum(a/b)

#7matrix
#7.1
B = matrix(c(10, -10, 10), nrow = 15, ncol = 3)
BB = t(B)%*%B

#7.2
x <- matrix(c(1, 1, 3, 5, 2, 6, -2, -1, -3), nrow = 3, ncol = 3, byrow = TRUE)

Odderad <- function(x){
  for (i in 1:dim(x)[1]){
    for (j in 1:dim(x)[2]){
      if(x[i, j]%%2 != 0){
        x[i, j] = x[i, j]*2
      }
    }
    
  }
  return(x)
}

#8.functions
#8a.1
x <- c(1, 2, 3)
tmpFn1 <- function(x){
  
  for (i in 1:length(x)){
    x[i] = x[i]^i
  }
  return(x)
}
#8a.2
tmpFn2 <- function(x){
  
  for (i in 1:length(x)){
    x[i] = (x[i]^i)/i
  }
  return(x)
}

#8b.1
tmpFn3 <- function(x, n){
  sum = 1
  for (i in 1:n){
   sum = sum + (x^i)/i
  }
  return(sum)
}
