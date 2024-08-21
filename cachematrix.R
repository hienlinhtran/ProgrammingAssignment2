#makeCacheMatrix Function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the cache for the inverse
  
  # Function to set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear the cached inverse when matrix is updated
  }
  
  # Function to get the matrix value
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


#cacheSolve function
cacheSolve <- function(x, ...) {
  # Check if the inverse is already cached
  inv <- x$getInverse()
  
  # If cached, return the cached inverse
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setInverse(inv)
  
  inv
}