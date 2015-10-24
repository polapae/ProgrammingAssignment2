## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #set default value for inverse matrix
  set <- function(y) { #set function that assigns x to y and reset the matrix
      x <<- y
      m <<- NULL
  }
  get <- function() x  #Get a matrix
  setInverse <- function(solve) m <<- solve  #Set an inverse matrix
  getInverse <- function() m   #Get an inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    #Get the matrix from cache
    m <- x$getInverse()
    #Check the matrix from cache whether it has been calculated
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)
    }
    #if not it'll calculate the inverse matrix
     data <- x$get()
     m <- solve(data, ...)
     #set matrix to cache
     x$setInverse(m)
     m
      ## Return a matrix that is the inverse of 'x'
}
