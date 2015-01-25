# makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
#1  set the matrix
#2  get the matrix
#3  set the inverse
#4  get the inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL 
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get<- function() x
 setinverse <- function(solve) m <<- solve
 getinverse <- function() m
 list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


##  cacheSolve function calculate the inverse of the matrix, returned by
## makeCacheMatrix function. cacheSolve should take cached results in order to
## make further calculations faster.

cacheSolve <- function(x, ...) {
  m <-x$getinverse()   
  if(!is.null(m)) { 
    message("getting cached data")
    return(m)
  }
  cached <- x$get()
  m<- solve(cached,...)
  x$setinverse(m)
  m
}
## Return a matrix that is the inverse of 'x'
