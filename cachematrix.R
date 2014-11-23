## In combination, cacheSolve interacts with the object that results from 
## makeCacheMatrix in order to avoid duplicating work

## the makeCacheMatrix function creates an object that stores attributes of a matrix, including
## the matrix value itself.  It has inner functions that retrieve these attributes.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the matrix object, but first it checks the
## object to see if it already has an inverse stored.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get())
  x$setinverse(inv)
  inv
}
