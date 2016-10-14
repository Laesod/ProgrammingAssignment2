## 
## Francois-David Lessard
## fdlessard@gmail.com
##
## Assignment 2: Caching the Inverse of a Matrix
## 
## These 2 fucntions are used to cache and calculate the inverse of a square matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(mat_inv) inv <<- mat_inv
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  nbOfRows <- nrow(data)
  nbOfCols <- ncol(data)
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
