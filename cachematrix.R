## Put comments here that give an overall description of what your
## functions do

## Creates an object consisting of a list of methods to get and set a matrix X, and to retrieve and store a cached inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
  #Create empty cache:
  inv <- NULL
  #Set function.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #Get Function.
  get <- function() x
  #Get/Set inverse function.
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  #Return list:
  list(set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


## Writes the inverse of a matrix to a makeCacheMatrix object. Or just retrieves it if it already exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##Check if cache exists.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached data...")
    return(inv)
  }
  #Otherwise calculate the inverse.
  data <- x$get()
  inv <- solve(data,...)
  #store result in cache.
  x$setinverse(inv)
  #return result:
  inv
}
