## makeCacheMatrix creates a special "matrix" object that can cache its inverse 

## Following 4 methods are defined on the object
## set() - set the value of the matrix
## get() - the value of the matrix
## setinv() - the value of the inverse of the matrix
## getinv() - the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) x_inv <<- inverse
  getinv <- function() x_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

## Usage: 
## matrix <- rbind(c(1, -1/4), c(-1/4, 1))
## specialMatrix <- makeCacheMatrix(matrix)
## cacheSolve(specialMatrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}


