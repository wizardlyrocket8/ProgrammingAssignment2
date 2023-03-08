# Creation of matrix object
makeCacheMatrix <- function( m = matrix() ) {
  
  # Initialization
  j <- NULL
  
  # Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    j <<- NULL
  }
  
  # Get the matrix
  get <- function() {
    # Return the matrix
    m
  }
  
  # Inverse of the matrix
  setInverse <- function(inverse) {
    j <<- inverse
  }
  
  # Inverse of the matrix
  getInverse <- function() {
    # Returns the inverse
    j
  }
  
  # Returns a list of methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# Computes the inverse of the matrix returned by "makeCacheMatrix"
# If the inverse has already been calculated (and the matrix has not
# changed), then the "cachesolve" retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  # Get the matrix from our object
  data <- x$get()
  
  # Calculation of the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  # Set the inverse to object
  x$setInverse(m)
  
  # Returns the matrix
  m
}