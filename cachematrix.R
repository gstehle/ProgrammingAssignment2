## These functions calculate, cache and retrieve the inverse of a matrix.

## This function takes a matrix as its input and returns a list of four functions to set and get 
## the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
   
   inv <- NULL
    
   set <- function(y) {
      x <<- y
      inv <<- NULL }
    
   get <- function() x
   setinv <- function(solve) inv <<- solve
   getinv <- function() inv
   list (set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function takes a matrix x as its argument and returns either the cached value or computes
## and returns the inverse.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
