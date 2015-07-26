##  Two functions to cache thw inverse of matrix.

## Function makeCacheMatrix will create object (m) where inverse values will be cached
makeCacheMatrix <- function(x = matrix()) {
  m <- data.frame()
  set <- function(y){
    x <<- y
    m <<- data.frame()
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## Function cacheSolve compute inverse of object, return it with message "Computed
## inverse data". If there is already computed inverse, the inverse will be 
## returned form the cache together with message "Cashed inverse data".

cacheSolve <- function(x, ...) {
  m <- x$getinverse
  if(!is.null(m)){
    message("Cashed inverse data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  message("Computed inverse data")
  m
}

