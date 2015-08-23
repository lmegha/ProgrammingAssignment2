## The functions below provide a faster computation for matrix inversion using caching as its main logic.

## This function creates a matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
inverse <- NULL
  set <- function (z) {
      x <<- z
      inverse <<- NULL
  }
  
  get <- function () {
      x
  }
  
  inx <- solve(x)
  
  setinverse <- function (inx) {
      inverse <<- inx
  }
  
  getinverse <- function () {
      inverse
  }
}


## Returns a computed inverse of the matrix returned by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
	
	inverse <- x$getinverse
    if(!is.null(inverse)) {
        message("Getting cached data!")
        return(inverse)
    }
    
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
       
}
