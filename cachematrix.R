## Put comments here that give an overall description of what your
## functions do

## This function caches the result of a matrix inverse
## operation
makeCacheMatrix <- function(x = matrix()) {
  cachedInv <- NULL
  set <- function(mdat) {
    x <<- y
    cachedInv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) cachedInv <<- inv
  getinv <- function() cachedInv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function provides the inverse of the matrix
## x, either from the cache, if it has been previously
## computed, or by computation if not
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    # Uncomment the below line to debug-by-print
    # in case you suspect the cached value
    # isn't being used
    # message("Using cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}
