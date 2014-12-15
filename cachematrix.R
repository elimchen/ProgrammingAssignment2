# Matrix inversion is usually a costly computation.  The functions below are created to catche the inverse of a matrix 
# and avoid repeatly  & costly computation.
#
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# 1)  set the value of the matrix
# 2)  get the value of the matrix
# 3)  set the value of the inverse
# 4)  get the value of the inverse
##
## function makeCacheMatrix creates a special "matrix" object that can cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inv <- function(inverse) inv <<- inverse
  get_inv <- function() inv
  list(set = set, get =get,
       set_inv = set_inv,
       get_inv = get_inv)
}

##
## function cacheSolve computes the inverse of the special "matrix" returned by make
## 
cacheSolve <- function(x, ...) {
  inv <- x$get_inv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inv(inv)    
  inv
}


##~~~~~~~~~~~~~~ Test Output ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
##  source("cachematrix.R")
##> x <- matrix(rnorm(4),2,2)
##> m <- makeCacheMatrix(x)
##> m$get()
##[,1]       [,2]
##[1,] -0.1893073 -0.1427265
##[2,]  0.4089911  1.8376961
##> cacheSolve(m)
##[,1]       [,2]
##[1,] -6.347491 -0.4929842
##[2,]  1.412675  0.6538764
##> cacheSolve(m)
##getting cached data
##[,1]       [,2]
##[1,] -6.347491 -0.4929842
##[2,]  1.412675  0.6538764
##> 
##  > x <- matrix(1:4,2,2)
##> m <- makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
