## Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly
##The functions below store a cache of the computed inverse of a matrix for future usage. 

##The function makeCacheMatrix Creates a list containing a function to do the following:
##1) Set the value of the inverse
##2) get the value of the matrix
##3) set the value of the inverse of the matrix
##4) get the value of the inverse of the matrix
makeCacheMatrix=function(x=matrix()) {
  Q <- NULL
  set <- function(y) {
    x <<- y
    Q <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Q <<- inverse
  getinverse <- function() Q
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The cachesolve function checks to see if the inverse 
##of the matrix is available in the cache, and if it is retrieve and display it + skip computation. 
##If the inverse of the matrix is not available, the inverse is calculated and displayed, +
##stored in the cache. 

cachesolve <- function(x, ...) {
  Q <- x$getinverse()
  if(!is.null(Q)) {
    message("getting cached data")
    return(Q)
  }
  data <- x$get()
  Q <- solve(data)
  x$setinverse(Q)
  Q
}
