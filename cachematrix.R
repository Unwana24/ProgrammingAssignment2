## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix creates a matrix, which is actually a list containing a function to
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The function below solves the inverse of the special matrix returned by the makeCacheMatrix above. 
##cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}