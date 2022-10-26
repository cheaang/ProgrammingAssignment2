## To cache the inverse of a matrix rather than compute it repeatedly 
## which will increase the computation costs 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function will computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

## Step to test it
## testMatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## testMatrix$get() - return the matrix set
## testMatrix$getInverse() - return NULL 
## cacheSolve(testMatrix) - return the matrix inverse
## testMatrix$getInverse() - return the cache matrix inverse
