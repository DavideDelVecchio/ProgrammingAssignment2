## The makeCacheMatrix constructs a suitable object that can store the value 
## of both the direct and cached inverse it should be used in combination with cacheSolve
## that when called before perforimg the computation of the inverse checks for the cached variable
## if not found computes the inverse and caches it in the object returned by makeCacheMatrix
## usage example
## > set.seed(44)
## > m <- matrix(sample.int(100,size=9,replace=TRUE), nrow=3)
## > d <- makeCacheMatrix(m)
## > cacheSolve(d)
## > subsequent  invocation returns cached data
## > inv <- cacheSolve(d)
## returning cached data

## makeCacheMatrix stores in variable i the inverse and provides accessor methods for both 
## the data (ie the matrix one wants to calculate the inverse) and the inverse itself

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve searches for a cached value of the inverse if found returns it otherwise 
## computes it by the means of the standard solve method and stores the inverse for future invocations


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv)){
        message("returning cached data") 
        return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
