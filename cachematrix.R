## This R file lets user cache the inverse of a matrix
        ## functions do

## This function creates a special matrix object that cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverseMatrix <- function(inverse) inverseMatrix <<- inverse
  getinverseMatrix <- function() inverseMatrix
  list(set = set, get = get, setinverseMatrix = setinverseMatrix, getinverseMatrix = getinverseMatrix)
}


## This function deals with th computation of inverse returned from the function above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getinverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached result")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setinverseMatrix(inverseMatrix)
  inverseMatrix
}
