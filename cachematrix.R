## cachematrix provides utility functions to cache the inverse of a matrix 
## in-memory.  Since matrix inversion is typically a costly computation, 
## caching the result can yield improved runtime performance for repeated
## inverse invocations.

## Returns an enriched matrix that exposes operations to write/read cached
## version of the inverse of the supplied matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  list(
    set = 
      function(y) {
        x <<- y
        inverse <<- NULL
      },
    get = function() x,
    setinverse = function(i) inverse <<- i,
    getinverse = function() inverse)
}


## Computes the inverse of the supplied enriched matrix.  This function 
## leverages the enriched matrix cache to avoid repeated inverse computations.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (is.null(inverse)) 
    x$setinverse(solve(x$get(), ...))
 
  x$getinverse()
}
