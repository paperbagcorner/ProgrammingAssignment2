## This file contain a function to construct a special object CacheMatrix, which
## is a matrix with the ability to cache its inverse, and a function which
## operates on a CacheMatrix and computes its inverse and store it in its cache.

## The function makeCacheMatrix creates a special "matrix". It returns a list
## which contains functions to
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The function cacheSolve returns the inverse of the CacheMatrix x. It returns
## the cached inverse if it exists. Otherwise the inverse is computed and
## cached.
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }

    inverse <- solve(x$get(), ...)
    x$setinverse(inverse)

    inverse
}
