## makeCacheMatrix wraps a matrix in a function scope. 
## Args
## x - input can be any matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x

  getinv <- function() {
    if(is.null(inv)) inv <<- solve(x)
    inv
  }

  list(set = set, get = get, getinv = getinv)
}


## Function return calculated inverse, if it's already caclulated then it grabs from the scope
## Args:
##    x - a matrix wrapped with makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x$getinv()
}
