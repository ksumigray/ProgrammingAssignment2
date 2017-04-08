## Cache the inverse of a matrix
## 

## The function will create a "special" matrix object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


## Identifies the inverse of the special matrix created above. First 
## checks to see if the inverse has already been calculated. If so, it pulls
## it from cache. Otherwise, it solves the matrix and sets the value via the
## setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
