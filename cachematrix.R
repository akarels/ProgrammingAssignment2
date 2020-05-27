## Put comments here that give an overall description of what your
## functions do
## These function implement functions to calculate the inverse
## of a matrix an cache the result.

## Write a short comment describing this function
## makeCacheMatrix will create a matrix object which
## can cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve will take as input a matrix cache object (as
## created by the makeCacheMatrix function) and solves for
## the inverse of the matrix. If the inverse as not already
## been calculated, the function will calculate the inverse
## and cache the result. The cache result will be returned.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
