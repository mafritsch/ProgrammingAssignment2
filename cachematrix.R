## makeCacheMatrix and cacheSolve demonstrate the usage of lexical scoping
## for caching values, which are expensive to calculate.
## makeCacheMatrix creates an object that contains a matrix and its inverse
## and provides getter and setter functions to access the data.
## cacheSolve takes the object created by makeCacheMatrix and either takes
## the inverse of the matrix from cache or calculates it.

## makeCacheMatrix takes a matrix as argument. It creates variables
## to store the data and the inverse in its closure. It provides
## getter and setter functions to access the data inside the object.
## It returns a list with the access functions which stores the 
## data in its environment.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = setmatrix, get = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cachesolve takes a list created with makeCacheMatrix as an argument.
## It checks if makeCacheMatrix already contains the inverse of the 
## contained matrix. If the inverse exists it is returned. Otherwise, 
## the inverse is calculated, set in the object made by makeCacheMatrix 
## and the inverse is returned.

cacheSolve <- function(x, ...) {
    ## Loads the inverse from the cache
    inverse <- x$getinverse()
    ## If inverse is in cache, return the value
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## Take the matrix data and calculate the invers
    data <- x$get()
    inverse <- solve(data)
    ## Set the inverse to the cache for further usage
    x$setinverse(inverse)
    inverse
}