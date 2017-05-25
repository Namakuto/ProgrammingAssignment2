#----------------------------------------------------------------------------
# This script will let you input (and cache) a matrix then return its inverse.


# To begin, create a variable that will store a call for makeCacheMatrix(). 
# makeCacheMatrix() should input all the appropriate arguments (in order) that are
# needed to construct your matrix of choice. (e.g, makeCacheMatrix(1:4, 2, 2))

# Use cacheSolve() to return the inverse matrix of makeCacheMatrix(). Input the 
# name of the variable (that stores makeCacheMatrix()) within the brackets of 
# cacheSolve().

makeCacheMatrix <- function(x = matrix(a,b,c)) {
  i <- NULL
  
    set <- function(y = matrix(a,b,c)) {
      x <<- y
      i <<- NULL }
    
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


# Inversion solving of the matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get() # getting the matrix values 
  i <- solve(data, ...) # inversion
  x$setinverse(i)
  i
}

