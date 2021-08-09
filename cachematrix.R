## This is the second assignment of R course 
## It has two functions to cache the inverse of the matrix 


## This function creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y){
    x <<- y 
    inv <<- NULL 
  }
  get <- function() x 
  
  setInv <- function(solve){inv <<- solve}
  getInv <- function() inv
  
  list(set=set, get=get, 
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the matrix or read it from the cache if exists 

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if (!is.null(inv)){
    message ("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv 
  
}
