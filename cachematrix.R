## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(n) {
    m <<- n
    inv <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(m, ...) {
  inverse <- m$getInverse()
  if(!is.null(inverse)) {
    message("wait while data is retrieved from cache")
    return(inv)
  }
  data <- m$get()
  inverse <- solve(data)
  m$setInverse(inverse)
  inverse
}

