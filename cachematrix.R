## Put comments here that give an overall description of what your
## functions do

## This function creates a special "vector", which is really a list containing a function to
### set the value of the matrix
### get the value of the matrix
### set the value of the inverse matrix
### get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setminv <- function(minv) m <<- minv
  getminv <- function() m
  list(set = set, get = get,
       setminv = setminv,
       getminv = getminv)
}


## This function checks whether the inverse for the matrix is already available in the cache
## and if yes returns it. If not, it computes the inverse using solve function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getminv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setminv(m)
  m
}
