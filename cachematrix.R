## These functions provide a mechanism for caching the inverse of a matrix.
## This is useful for computationally expensive matrix operations, as it avoids 
## redundant calculations.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y  # Assign the input matrix to the parent environment
    inv <<- NULL  # Reset the inverse as the matrix has changed
  }
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse of the matrix
  getinverse <- function()inv
  
  # Return a list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x,...) {
  inv <- x$getinverse()  # Try to get the cached inverse
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If not cached, calculate the inverse
  data <- x$get()  # Get the matrix
  inv <- solve(data, ...)  # Calculate the inverse using solve()
  x$setinverse(inv)  # Cache the inverse
  inv  # Return the inverse
}

