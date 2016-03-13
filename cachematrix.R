## In makeCacheMatrix the inverse of a matrix is cached. This is done because
## matrix inversion is a costly computation, which can be avoided in this way,
## if it needs to be done repeatedly.

## The function, makeCacheMatrix, creates a special matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated and the
## matrix has not been changed, then the cachesolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
