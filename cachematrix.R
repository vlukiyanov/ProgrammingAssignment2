## Cache object for a matrix inverse, and a initialisation function called makeCacheMatrix and a cacheSolve
## function which takes a cache object and returns the inverse, example usage:
##   x <- makeCacheMatrix(matrix(c(c(1, 0), c(10, 1)),2,2))
##   ix <- cacheSolve(x)
##   ix == matrix(c(c(1, 0), c(-10, 1)),2,2) 
## the final line will return a matrix of TRUE

## Given a matrix x, create a special object which can cache the inverse; this object is initialised with 
## the matrix and has 4 methods:
## $get() to get the value of x
## $set(y) to set the value of xto y
## $getinverse() to get the inverse if it has been computed, otherwise returns NULL
## $setinverse(i) to set the inverse and store it within the object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a special cached matrix created by makeCacheMatrix, return the inverse, either from the cache if it
## has already been computed or by populating the cache and then returning

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
