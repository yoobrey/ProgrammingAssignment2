## These two functions offer solving for the inverse of a matrix with the
## ability to cache their values, allowing for a more efficient use in other
## computation-heavy processes by storing and retrieving correctly computed
## values instead of recalculating them every time they are needed.
##
## makeCacheMatrix() creates a "list of functions" that can cache and retrieve
## the contents of an original data matrix "x" and its inverse. It should be
## assigned to an object and be given a matrix to store. The functions in the
## "list" can be called through the $ (dollar sign) operator.
## 
## cacheSolve() returns the inverse of matrix stored in the object created by 
## "makeCacheMatrix". It tries to retrieve a stored inverse, and if it does not
## exist it calculates the inverse and stores it into through object created by
## "makeCacheMatrix".


## This function creates a set of functions for caching and retrieving matrices,
## and return the set of functions as a list.

makeCacheMatrix <- function(x = matrix()) {  # Also initializes empty matrix "x"
  inverse <- NULL  # Initialize "inverse"
  
  ## "setmatrix" can be used to replace the matrix in its containing object and
  ## reset the value of "inverse".
  setmatrix <- function(y) {
    x <- y
    inverse <- NULL
  }
  
  ## "getmatrix" fetches the contents of the original matrix "x".
  getmatrix <- function() {
    return(x)
  }
  
  ## "setinverse" stores the inverse "inv" of matrix "x" into "inverse".
  setinverse <- function(inv) {
    inverse <<- inv
  }
  
  ## "getinverse" fetches the stored inverse of matrix "x".
  getinverse <- function() {
    return(inverse)
  }
  
  ## Return the functions as a list object.
  l <- list(setmatrix = setmatrix,
            getmatrix = getmatrix,
            getinverse = getinverse,
            setinverse = setinverse)
  return(l)
}

## This function returns the cached inverse of a matrix, or solves for it if it
## does not yet exist.

cacheSolve <- function(x, ...) {
  ## Fetch the stored inverse and return it if it exists.
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message('Cached inverse exists. Retrieving...')
    return(inverse)
  }
  
  ## Otherwise, compute for the inverse of and store it.
  matrix <- x$getmatrix()
  inverse <- solve(matrix)  # This assumes that "matrix" is an invertible matrix
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'.
  return(inverse)
}
