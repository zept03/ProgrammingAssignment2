## The makeCacheMatrix function creates a special matrix, while the cache solves 
## calculates the inverse of the matrix. If the inverse of the matrix 
## is already calculated, it will find the answer to the cache and return it.

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  temp <- NULL
  set <- function(y) {
    x <<- y
    temp <<- NULL 
  }
  get <- function() x
  #Function to set the inverse
  setInverse <- function(inverse) temp <<- inverse
  #function to get the inverse
  getInverse <- function() temp
  # Return list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function computes the inverse of the special matrix. If the inverse
## has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  temp <- x$getInverse() # Get the cached value
  if(!is.null(temp)) { # If the cache is not empty
    message("Please wait. We will get the cached data")
    return(temp)
  }
  # If the cache is empty
  data <- x$get()  # Get value of matrix
  temp <- solve(data) # Calculate inverse
  x$setInverse(temp)  # Cache the result
  temp                # Return the inverse
}
