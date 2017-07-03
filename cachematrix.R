
## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
      x <<- y
      M <<- NULL
  }
  get <- function() x
  setInv <- function(inverse)  M <<- inverse
  getInv <- function() M
  list(set = set, get = get, 
       setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  M <- x$getInv()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- x$get()
  M <- solve(data, ...)
  x$setInv(M)
  M
}

