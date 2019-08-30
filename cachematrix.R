## The first function, makeVector creates a special "vector", which is really 
## a list containing a function to:
# set the value of the vector
# get the value of the vector
# set the value of the inverse
# get the value of the inverse

## The second function, cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

## Creating the function that can cache its inverse using a special
## "matrix" object:
  
## Essentially replacing "m" with "inv" and "mean" with "inverse" in
## the makevector function



makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          
  set <- function(y) {   ## setting the value of the vector
    x <<- y
    inv <<- NULL          
  }
  get <- function() x    ## getting the value of the vector
  setinverse <- function(inverse) inv <<- inverse   ## setting the value of the inverse
  getinverse <- function() inv          ## getting the value of the inverse
  list(set = set, get = get,      
       setinverse = setinverse,   
       getinverse = getinverse)
}



## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above.

## Essentially replacing "m" with "inv", "mean" with "inverse", and "mean" with 
## "solve" in the cachemean function which returns its inverse for a square matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {               ## if the matrix is invertible, return its inverse
    message("getting cached data")
    return(inv)       
  }
  data <- x$get()
  inv <- solve(data, ...)   ## solving the inverse of the data instead of computing the mean
  x$setinverse(inv)         ## setting the value of the data in the cache with setinverse
  inv
}
