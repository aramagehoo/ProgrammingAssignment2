## The 2 functions makeCacheMatrix and cacheSolve work together to set, store and solve for the inverse 
## of a square invertible matrix.

## makeCacheMatrix is first run to create a list which will be passed to the function cacheSolve
## the list contains the values to set up the cache for the matrix. It sets and gets the value of the function
## as well as the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
                  m <- NULL
                  set <- function(y) {
                        x <<- y
                        m <<- NULL
                  }
                  get <- function()x
                  setmatrix <- function(solve)m <<- solve
                  getmatrix <- function()m
                  list(set = set,get=get,
                       setmatrix = setmatrix,
                       getmatrix = getmatrix)
}


## The cacheSolve function returns the inverse of the matrix passed. If the matrix has been passed to the function
## makeCacheMatrix previously, the cached answer is returned. If the matrix has not been passed, then the function 
## cacheSolve solves for the inverse of of the matrix and returns the anser. The original matrix passed is  then cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getmatrix()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrix(m)
      m
}
