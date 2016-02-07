  
 -makeCacheMatrix <- function(x = matrix()) {
 ### Function 1: creating a "matrix" that can cache its inverse.
  
 makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
     x <<- y
     inv <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
   list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }
  
 
  
  cacheSolve <- function(x, ...) {
 -        ## Return a matrix that is the inverse of 'x'
   inv <- x$getinverse()
   if(!is.null(inv)) {
     message("getting cached data.")
     return(inv)
   }
   data <- x$get()
   inv <- solve(data)
   x$setinverse(inv)
   inv
  }
 
 ### Post the initial commands -- running a test
 x = rbind(c(1, -1/4), c(-1/4, 1))
 m = makeCacheMatrix(x)
 m$get()
 
 ### first run for cache
 cacheSolve(m)
 
 cacheSolve(m)
