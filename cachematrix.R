## This is R Programming Assignment for Week 3

## Returns an inverse of a "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Computes the inverse of the "matrix" returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}

B <- matrix(c(1,2,3,4),2,2)

B1 <- makeCacheMatrix(B)
cacheSolve(B1)

cacheSolve(B1)

