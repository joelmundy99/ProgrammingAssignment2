## These functions can be used to cache the inverse of a matrix, which can be used instead 
## of repeatedly computing the inverse.

## The function below creates a special "matrix" obkect that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) k <<- inverse
  getinverse <- function() k
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returbed by the function above
## If the inverse has already been calculated (and the matrix is unchanged),
## then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  k <- x$getinverse()
  if (!is.null(k)) {
    message("getting cached data")
    return(k)
  }
  data <- x$get()
  k <- solve(data, ...)
  x$setinverse(k)
  k
}