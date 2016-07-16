## We define a "matrix" object that can cache its inverse, and an inversion
## method which, before performing the inversion, checks whether it has already
## been computed, and returns the cached value in that case 

## makeCachematrix creates a matrix "object" with the possibility of caching its
## inverse in the field inv. Access methods are defined as a list.

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


## cacheSolve computes the inverse of a matrix object. If the inverse has been
## cached, it returns that value. Otherwise it computes the inverse using solve,
## stores it in the inv field of the matrix object, and returns the value.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
