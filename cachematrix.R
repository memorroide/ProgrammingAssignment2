## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that con cache it?s inverse.

makeCacheMatrix <- function(x = matrix()) {
  #initiatilaze j
  j <- NULL
  #create the matrix
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  #get value
  get <- function() x
  #invert the matrix
  setInverse <- function(inverse) j <<- inverse
  #get inverted matrix
  getInverse <- function() j
  
  #return created functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #attempt get inverse of the matrix in cache
  j <- x$getInverse()
  #if exists, return inverted matrix
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat, ...)
  x$setInverse(j)
}
