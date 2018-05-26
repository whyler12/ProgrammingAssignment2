## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# Creates an object that can cache its inverse and which 
# contains 4 functions to get and set the input matrix and its inverse respectively

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<-function(y){
    x<<-y
    inv <- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  return(list(get = get, 
              setinverse = setinverse,
              getinverse = getinverse))
}

## Write a short comment describing this function
# Computes the inverse of the input matrix.If the inverse is already calculated,then the inverse is retrieved from the cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
