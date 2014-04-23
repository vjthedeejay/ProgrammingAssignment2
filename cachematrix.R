## This file contains a set of functions that can calculate,
## cache, and retrieve the value of the inverse of a matrix.

## makeCacheMatrix <- function(x = matrix())
## This function creates a special "matrix" object that is a list
## containing a function to 1) set the value of the matrix,
## 2) get the value of the matrix, 3) set the value of its inverse,
## and 4) get the value of its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve <- function(x, ...)
## This function computes the inverse of the matrix returned
## by the makeCacheMatrix function. If the inverse has already
## been computed, this function retrieves the value from the 
## cache. Otherwise, it calculates the inverse and then sets
## this value in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
