## Overall the functions produce the inverse of a square matrix.
## If the inverse has already been computed, that value is cached,
## which will save in computation time.

## This creates a special "vector" which is really a list containing
## a function to get & set the matrix, then get & set the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


##  This calculates the inverse of the matrix, but if the inverse
## is already calculated it gets it from the cache and does not need to re-compute.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
