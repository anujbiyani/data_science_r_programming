## Generates a cacheMatrix object with functions to set the matrix, get it, set the inverse, and get the inverse.

## Sample usage:
## cacheMatrix <- makeCacheMatrix()
## cacheMatrix$set(matrix(runif(9, 1, 10), nrow=3, ncol=3))
## cacheSolve(cacheMatrix)
### returns the inverted matrix and caches the result
### subsequent invocations with an unchanged matrix will return the cached result

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Inverts the matrix and caches the result. If called with the same matrix, then the result will be returned from the cache.

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
