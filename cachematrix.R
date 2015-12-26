## The two functions herein cache the inverse of a Matrix. 
## Should the inverse of the matrix exist in the cache no recalculation is needed
## The user profits especially for large matrices from a cut in computing time.

## The function makeCacheMatrix sets the value of matrix, gets the value of matrix.
## Then it sets a value for the respective inverse matrix, and gets the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function cacheSolve checks whether the calculated inverse of the matrix is stored in the cache.
##Should there be no calculation in the cache it recalculates and saves it in the cache, if it exists it takes the existing calculated value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
