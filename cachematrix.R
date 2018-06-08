##Programming Assignment, Week 3 
##The cachesolve function returns the inverse of a square matrix.
##This data is pulled from the cache if possible. The makeCacheMatrix provides
##the information necessary to pull data from the cache.
##Here is an example of how to use the functions together:
  ## y<-makeCacheMatrix(x)
  ## cachesolve(y)

##The makeCacheMatrix function creates the matrix of information that is needed to
##pull data from the cache.

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

##The cachesolve function returns the inverse of the matrix when provided the data determined
##by the makeCacheMatrix function. The cachesolve functions draws the inverse of the matrix
##from the cache if available. If not available, it calculates the inverse anew.

cachesolve <- function(x, ...) {
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