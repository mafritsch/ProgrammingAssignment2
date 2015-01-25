## These two functions define an object that stores a matrix and its inverse 
## and a function that reads and sets the cache of the object with the inverse matrix.



## The makeCacheMatrix function creates a list that stores four functions
## to set and get a matrix and its inverse from the context of the makeCachMatrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve functions takes a list created with makeCacheMatrix  and checks 
## if it already contains the inverse matrix. If not it calculates the inverse and 
## sets it into the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
