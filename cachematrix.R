# Based on the vector mean example

# Creates a wrapper around the given matrix with four operations:
# * set(matrix) - set the matrix to wrap (can be done be function argument as well), resets prevously calculated inverse,
# * get() - get the wrapped matrix,
# * getinverse() - get the cached inverse matrix,
# * setinverse(matrix) - set the cached inverse matrix (should be invoked only be cacheSolve, see below).
makeCacheMatrix <- function(x = matrix()) {
  # state:
  inv <- NULL
  
  # "methods":
  
  # get matrix
  get <- function() x
  
  # (re)set matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get inverse
  getinv <- function() inv
  
  # set inverse (use carefully)
  setinv <- function(inverse) inv <<- inverse
  
  #result list
  list(
    set=set,
    get=get,
    setinverse=setinv,
    getinverse=getinv)
}


# Calculates the inverse of a matrix. If the inverse has been previously calculated, the cached value is returned instantly.
# x - a list created by makeCacheMatrix
cacheSolve <- function(x, ...) {
  cachedinv <- x$getinverse()
  ret <- NULL
  
  if(is.null(cachedinv)) {
    message("calculating")
    x$setinverse(solve(x$get()))
    ret <- x$getinverse()
  } else {
    message("from cache")
    ret <- cachedinv
  }
  
  ret
}
