## This is R Programming Assignment for Week 3

## Returns an inverse of a "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Computes the inverse of the "matrix" returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  ## Returns the inverse of the cache matrix
  inv <- x$get_inverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  inv
}

B <- matrix(c(1,2,3,4),2,2)

B1 <- makeCacheMatrix(B)
cacheSolve(B1)

cacheSolve(B1)

