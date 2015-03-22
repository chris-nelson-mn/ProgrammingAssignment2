## These functions allow for creating a special object
## that stores a matrix and can cache the value of its inverse. 
## The inverse-producing function calculates the inverse of 
## the matrix once, then caches the inverse so that it can be 
## retrieved multiple times without having to perform the 
## calculation each time.
##
## NOTE: We assume that the matrix provided is always
## invertible. No checking is done for this condition,
## and no error handling is provided.

## Description:
## Create an object that can store both a matrix
## and its calculated inverse, and return either when
## requested.
##
## Usage: makeCacheMatrix(x = matrix())
##
## Arguments: x, a matrix. Default is an empty matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setMatrix <- function(new_mtrx) {
        x <<- new_mtrx
        inverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Description:
## Given an object produced by makeCacheMatrix,
## produce the inverse of the matrix stored in the
## object. Calculates the inverse once and caches
## the value in the object, subsequent calls simply
## return the cached value.
##
## Usage: cacheSolve(x)
##
## Arguments: x, an object returned by a call to makeCacheMatrix
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message('retrieving cached inverse')
        return(inv)
    }
    i <- solve(x$getMatrix())
    x$setInverse(i)
    return(i)
}

