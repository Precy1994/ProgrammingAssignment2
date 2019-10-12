## Functions to create a special kind of matrix that can cache its inverse to avoid repeated calculations,
## and to calcualte the inverse if necessary

## This function holds information on a given matrix, x, and its inverse inv (if this has been calculated). Can
## set or retrive the values of the inverse and matrix as necessary. Assumes it is only passed invertible matrices.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function pulls an inverse from the CacheMatrix object, or calculates it if it has not previously been calculated.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

