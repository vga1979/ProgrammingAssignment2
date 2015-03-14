## The first function creates the inerts a matrix and caches it.
## The second function checks to see if the matrix has been inverted or not.  And if not, it inverts it.

## Create a list containing a function that sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse of the matrix, and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {   
  m <- NULL                                   
  set <- function(y) {                        
    x <<- y                                   
    m <<- NULL                                
  }
  get <- function() x                         
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## First check to see if the inverse of the matrix has already been calculated.
## If it has been calculated, then the function pulls the inverse matrix from the cache.
## If it has not been calculated, then the function proceeds to calculate the inverse of the matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setsolve(m)
  m
}
