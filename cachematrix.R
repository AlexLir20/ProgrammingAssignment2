## Functions to cache the inverse of a matrix
## Create a special matrix object to cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  ## Initialize the inverse
  a <- NULL
  
  ## Set the matrix
  set <- function( matrix ) {
    b <<- matrix
    a <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## Return the matrix
    b
  }
  
  ## Set the inverse of the matrix
  SetInverse <- function(inverse) {
    a <<- inverse
  }
  
  ## Get the inverse of the matrix
  GetInverse <- function() {
    ## Return the inverse
    a
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       SetInverse = SetInverse,
       GetInverse = GetInverse)
  
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of x
  
  ## Return a matrix that is the inverse of x
  b <- x$getInverse()
  
  ## Return the inverse if its already set
  if( !is.null(b) ) {
    message("getting cached data")
    return(b)
  }
  
  ## Get the matrix from the object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  b <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(b)
  
  ## Return the matrix
  b
  
}
