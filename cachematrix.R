## Cache the inverse of a matrix if it's already computed

## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(invmatrix) im <<- invmatrix
  getinverse <- function() im
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse=getinverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix.  
## If the inverse has already been calculated, 
## retrieve it from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  lm <- x$getinverse()
  if(!is.null(lm)) {
    message("getting cached data")
    return(lm)
  }
  data <- x$get()
  lm <- solve(data, ...)
  x$setinverse(lm)
  lm
}