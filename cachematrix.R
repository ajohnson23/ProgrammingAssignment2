## Builds a list of functions that can store/retrieve
## a matrix and a matrix inverse from a cache.
makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(mat) {
    xInverse <<- NULL
    x <<- mat
  }
  get <- function() x
  setInverse <- function(inverse) xInverse <<- inverse
  getInverse <- function() xInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## If the inverse is already cached, return that.
## Otherwise, solve, cache, and return the inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInverse <- x$getInverse()
  if( !is.null(xInverse) ) {
    message("getting cached data")
    return(xInverse)
  }
  data <- x$get()
  xInverse <- solve(data)
  x$setInverse(xInverse)
  xInverse
}
