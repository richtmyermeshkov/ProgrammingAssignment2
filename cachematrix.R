## The following functions provide support for caching the inverse of
## a matrix in order to avoid the time-consuming re-calculation of an
## inverse each time it is required


## The following function provides a special matrix function that allows
## for the storage of both a matrix and its inverse, which can be accessed
## through special setter/getter functions returned as a list.

makeCacheMatrix <- function(x = matrix()) {

  xinv <- NULL
  
  set <- function(y){
    x <<- y
    xinv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) xinv <<- inverse
  
  getinverse <- function() xinv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Given the the list of functions returned by the special matrix function
## defined above, the following function will find the inverse of the matrix.
## It will calculate it and cache it if it has not already been calculated.
## If it has already been calcuated, the cached version is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  xinv <- x$getinverse()
  
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  
  data <- x$get()
  
  xinv <- solve(data)
  
  x$setinverse(xinv)
  
  xinv
  
}
