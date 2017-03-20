makeCacheMatrix <- function(x = matrix()) {
## This funciton wil cache the inverse of its matrix argument
  inv <- NULL ## Initialize inverse matrix as NULL
  set <- function(y) {
  ## Assign new value to matricies in parent environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x ## Returns value of matrix argument
  setinverse <- function(inverse) inv <<- inverse
  ## Computes inverse of matrix argument and assigns computed value to parent environment
  getinverse <- function() inv
  ## Gets value of inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  ## Necessary in order to refer to functions with $ operator
}

cacheSolve <- function(x, ...) {
  ## This function will check if the inverse matrix has been computed an cached
  ## If so, cached matrix is retrieved. If not, value is calculated and assigned
  ## via setinverse()
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
