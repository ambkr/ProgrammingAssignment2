## Put comments here that give an overall description of what your
## functions do

## This function makes a special MATRIX object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invernator <- NULL
  set <- function(y) {
    x <<- y
    invernator <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) 
    invernator <<- inverse
  getInverse <- function() invernator
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the special MATRIX returned by makeCacheMatrix.
## If the inverse has been calculated already and the matrix did not change, this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    invernator <- x$getInverse()
    if (!is.null(invernator)) {
      message("getting cached data")
      return(invernator)
    }
    mat <- x$get()
    invernator <- solve(mat, ...)
    x$setInverse(invernator)
    invernator
  }
        ## Return a matrix that is the inverse of 'x'