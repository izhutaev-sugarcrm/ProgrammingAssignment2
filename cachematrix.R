## Programming Assignment 2: Lexical Scoping
## cacheSolve function caches result of a "solve" function
## so we can save time/resources when calling "solve"
## on a same matrix mupltiple times

## Create cachable matrix object from matrix X
## returns a list of set, get, setInverse and getInverse methods

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(value) inverse <<- value
    getInverse <- function() inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Calculate Inverse on a maxtic X and cache
## function results for future usage

cacheSolve <- function(x, ...) {
    inverse = x$getInverse()

    # check inverse results already calculated (not NULL)
    if (!is.null(inverse)) {
        message("getting inverse matrix from cache")
        return (inverse)
    }

    # otherwise calculate inverse value and store it into "cache"
    m <- x$get()
    inverse <- solve(m, ...)
    x$setInverse(inverse)
    inverse
}
