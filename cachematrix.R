## Solve and store the inverse of a matrix rather than spending time recomputing the inverse.
## Two function below.
## The first creates a variable object to store a matrix.
## The second solves for the inverse of the matrix and places it into cache for quicker access instead of re-solving.

## Create a variable to store a matrix.
## Solve for the inverse of this matrix and place it into cache.
## Creates callable functions to set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Returns the inverse of a matrix from cache if available.
## If inverse is not available in cache, then the inverse is calculated and stored in cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
