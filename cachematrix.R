## makeCacheMatrix and cacheSolve, together, calculate and cache the inverse of a matrix.

## makeCacheMatrix creates a special matrix object that can cache a given Matrix and it's inverse.
## A list of 4 functions that can be used as commands for this object is also created.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve will check if the inverse of the matrix contained in an object x created 
## with the makeCacheMatrix function is cached. If so, it will return the inverse matrix, 
## else it will calculate the inverse matrix and cache it using the setInverse function 
## of the x object. 
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
