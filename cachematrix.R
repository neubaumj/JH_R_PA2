## ############################################################
## makeCacheMatrix -- Function to compute inverse matrix
## cacheSolve -- Function to get inverse matrix
## ############################################################

## ############################################################
## makeCacheMatrix -- Function to compute inverse matrix
##    get - get matrix
##    set - set matrix
##    setinverse - set inverse matrix (solve inverse)
##    getinverse - get inverse matrix
## ############################################################

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(s) i <<- s
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## ############################################################
## cacheSolve -- Function to get inverse matrix
##   Either from cache or computing
## ############################################################

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  message("computing new inverse matrix data")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


## ############################################################
##  Testing
## ############################################################

## test using R function as a baseline ########################
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
A
AI <- solve(A)
AI

## test function (should NOT be cached) ########################
A2 <- makeCacheMatrix(A)
A2I <- cacheSolve(A2)
A2I

## test function (should be cached) ##########################
A2I <- cacheSolve(A2)
A2I

## test function (should be cached) ##########################
A2I <- cacheSolve(A2)
A2I

## clear all variables
rm(list=ls())

# Done
