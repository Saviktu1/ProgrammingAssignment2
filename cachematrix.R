## MakeCacheMatrix creates a special kind of matrix with cache properties.
## cacheSolve uses functions defined by makeCacheMatrix to return inverted
## matrix efficiently by returning result from cache after computed once.

## makeCacheMatrix creates a special matrix object and return a list of functions.
## Only feasible parameter is a square invertible matrix to work correctly with
## cacheSolve.
## set -function stores parameter matrix.
## get -function returns parameter matrix.
## setinverse stores inverted matrix.
## getinverse returns inverted matrix.

makeCacheMatrix <- function(x = numeric()) {
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


## cacheSolve computes and returns inverted matrix. It uses makeCacheMatrix 
## object only and needs as a parameter square invertible matrix to work 
## correctly. If cacheSolve has already computed inverse matrix once before 
## it returns value from cache instead of computing it again.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    m
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}