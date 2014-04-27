## This file implements two functions: makeCacheMatrix and cacheSolve
## 1 - makeCacheMatrix: create a wrapper for a matrix previously calculated
## 2 - cacheSolve: return the inverse matrix value by re-using the cached 
## value only if it exists or else re-calculate the inverse.

## makeCacheMatrix: This function creates a special "matrix" object that
## can cache its inverse. It creates 4 functions to set and get the matrix
## object and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ### Initialize matrix inverse
  ## This function assigns the value "y" to our matrix "x"
  ## It also resets the value of the inverse
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get the value of our matrix "x"
  get <- function() x
  ## Assigns the matrix inverse
  setInverse <- function(solve) m <<- solve
  ## Returns the value of our matrix inverse
  getInverse <- function() m
  ## Return a list containing our 4 functions...
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: Takes as argument a list representing a special matrix
## and then the function computes the inverse of our special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the this function retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Get the currently stored matrix inverse from x
  m <- x$getInverse()
  ## If the inverse value is not missing then no need to re-calculate the inverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If the inverse calue is missing, then calculate the inverse
  ## and finally cache the inverse so that it can be reused next time
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

