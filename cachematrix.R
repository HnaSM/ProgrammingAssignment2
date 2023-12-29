## These functions in combination cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  t <- NULL
  set <- function(y)  {
    x <<- y
    t <<- NULL
  }
  get<- function() x
  settranspose <- function(solve) t <<- solve
  gettranspose <- function() t
  list(set = set, get = get,
       settranspose = settranspose,
       gettranspose = gettranspose)
}

## This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then cacheSolve should retrieve the inverse from 
#the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

    t <- x$gettranspose()
    if(!is.null(t)) {
      message("getting cached data")
      return(t)
    }
    data <- x$get()
    t <- solve(data, ...)
    x$settranspose(t)
    t
}
  
