## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix caches the inverse of the matxi
## Provides 4 functions set, get, setInverseMatrix and getInverseMatrix function
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  setInverseMatrix <- function(m) inverseMatrix <<- m
  getInverseMatrix <- function() inverseMatrix
  list(set=set, get=get, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function
## cacheSolve function computes the inverse of the matrix.
## If inverse already exists or is cached, it is returned directly
## else the inverse is computed
## This function expects matrix to be a square matrix otherwise it would fail
## rbind(c(1, -1/4), c(-1/4, 1), c(1, -1/4)) This matrix which is 3x2 would fail
## rbind(c(1, -1/4), c(-1/4, 1)) would work as it is 2x2 matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getInverseMatrix()
        if(!is.null(inverseMatrix)) {
          message("Using cached data...")
          return(inverseMatrix)
        }
        
        data <- x$get()
        inverseMatrix <- solve(data)
        x$setInverseMatrix(inverseMatrix)
        inverseMatrix
}
