## This module contains functions for creating and manipulating CacheMatrix objects.
##
## CacheMatrix objects (currently) support caching of the following computationally
## expensive operations:
## * matrix inverse
##
## A CacheMatrix object, say x, is created with the 'makeCacheMatrix' function.
## The inverse of a CacheMatrix object is obtained with the 'cacheSolve' function. 
##
## Example:
##> x <- makeCacheMatrix(diag(c(1, 2, 4)))
##> cacheSolve(x)
##Compute inverse of x
##[,1] [,2] [,3]
##[1,]    1  0.0 0.00
##[2,]    0  0.5 0.00
##[3,]    0  0.0 0.25
##> cacheSolve(x)
##Use cached value of inverse
##[,1] [,2] [,3]
##[1,]    1  0.0 0.00
##[2,]    0  0.5 0.00
##[3,]    0  0.0 0.25


## Create a CacheMatrix object.
##
## Implementation note:
## The resulting CacheMatrix object, say 'x', internally uses the following functions:
## * x$set.matrix(y)   Set y to be the matrix for 'x'
## * x$get.matrix()    Get the matrix for 'x'
## * x$set.inverse(y)  Set y to be the inverse of the matrix for 'x'
## * x$get.inverse()   Get inverse of the matrix for 'x'
makeCacheMatrix <- function(x = matrix()) {
  cached.inverse <- NULL
  set.matrix <- function(y) {
    x <<- y
    cached.inverse <<- NULL
  }
  get.matrix <- function() x
  set.inverse <- function(inverse) cached.inverse <<- inverse
  get.inverse <- function() cached.inverse
  
  list(set.matrix = set.matrix, 
       get.matrix = get.matrix, 
       set.inverse = set.inverse, 
       get.inverse = get.inverse)
}


## Returns the inverse of CacheMatrix object 'x'.
##
## 'x' must be a "CacheMatrix object" created by the function 'makeCacheMatrix'
##
## Implementation note: 
## If the inverse of 'x' was recently computed, the inverse is retrieved from
## the cached value.
cacheSolve <- function(x) {
  x.inverse <- x$get.inverse()
  if (is.null(x.inverse)) {
    message("Compute inverse of x")
    matrix <- x$get.matrix()
    x.inverse <- solve(matrix)
    x$set.inverse(x.inverse)
  }
  else {
    message("Use cached value of inverse")
  }
  x.inverse
}
