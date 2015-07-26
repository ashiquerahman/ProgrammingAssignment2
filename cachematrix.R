## Following two functions could be used to find the inverse of a square invertible matrix

## For 'makeCacheMatrix' function:
##    the argument 'x' is a square invertible matrix
##    it returns a list containing functions to:
##              i) set the matrix
##              ii) get the matrix
##              iii) set the inverse
##              iv) get the inverse
##    Note: this list is used as an input for 'CacheSolve' function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse =setinverse, getinverse = getinverse)

}


## For 'cacheSolve' function:
##    the argument x is the output got from the 'makeCacheMatrix' function;
##    so, 'makeCacheMatrix' could be called as an argument for 'cacheSolve' function
##    it returns an inverse matrix for the original matrix put as an argument in 'makeCacheMatrix' function

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  ## if the inverse has already been calculated
  if(!is.null(inv)) {
    ## gets it from cache and skips computation
    message("getting caced data")
    return(inv)
  }
  ## if not found in cache, computes inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
