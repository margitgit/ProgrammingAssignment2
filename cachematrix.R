# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=numeric()) { 

	mat <- NULL
	setCacheMatrix <-function(y) {
	x <<- y
      mat <<- NULL	
	} # end setCacheMatrix function

getCacheMatrix <- function()x
setinvertMatrix <- function(solve) s <<- solve
getinvertMatrix <- function() s
list(setCacheMatrix = setCacheMatrix,
     getCacheMatrix = getCacheMatrix, 
     setinvertMatrix =setinvertMatrix,
     getinvertMatrix =getinvertMatrix   )

} # end makeCacheMatrix function
	



# R function is able to cache potentially time-consuming computations
# matrix inversion can be a time consuming operation
# it can be looked up in the cache rather than computed
# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve
# the inverse from the cache.


cacheSolve <- function(x, ...) {
  s <- x$getinvertMatrix()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$getCacheMatrix()
  s <- solve(data, ...)
  x$setinvertMatrix(s)
  s
}
