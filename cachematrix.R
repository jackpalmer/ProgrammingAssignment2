## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Creates the cache variable
  m <- NULL
  #Creates the matrix in the working environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Retrieves the value of the matrix
  get <- function() x
  #Calculates the inverse of the matrix
  setinverse <- function(solve) m <<- solve
  #Retrieves the inverse of the matrix
  getinverse <- function() m
  #Sets the created functions in the working environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #Returns cached matrix
  m <- x$getinverse()
  #If cached matrix does not exist then creates matrix in working environment
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  #Return inverse of matrix
  m <- solve(data, ...)
  x$setinverse(m)
  #Display matrix
  m
}
