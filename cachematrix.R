## This R file contains makeCacheMatrix and cacheSolve functions.
## These two functions are used to cache the inverse of a matrix.


## The function makeCacheMatrix creates a special matrix object which can cache
## its inverse.

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


## The function cacheSolve computes and returns the inverse of the special 
## matrix returned by above makeCacheMatrix function. If the inverse has already
## been computed then the function would retrieve the inverse from the cache
## else it will compute the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Fetching cached data...")
        return(inv)
    }
    ## get value of matrix
	data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    return(inv)
}
