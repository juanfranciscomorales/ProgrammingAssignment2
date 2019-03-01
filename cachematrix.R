# makeCacheMatrix() returns a list of functions regarding cached inverse.
# cacheSolve() checks the cache for inverse of the matrix and calculates it, if required.

# This function return 4 functions described below:
# 1. set() function updates the matrix. This function is to be used whenever the matrix is to be changed.
#    The function also deletes the cached value of the inverse, if any.
# 2. get() function is used to check the original matrix for which the inverse is cached/will be calculated.
# 3. setinv() function will get the inverse as whatever is passed as an argument.
#    This function should not be called anywhere apart from cacheSolve() function.
# 4. getinv() function returns the cached inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function checks the cache for the inverse.
# If found, it will return it.
# Otherwise, it will calculate the inverse and return it after putting the value in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}