## In this module the ability to cache potentially time-consuming 
## calculations, such as inverse matrix calculation, is introduced.
## 'makeCacheMatrix' function loosely corresponds to a constructor
## of a class that encapsulates a matrix and its inverse and provides
## accessor functions for them.
## 'cacheSolve' function calculates an inverse of a matrix and saves it
## unless it was already calculated and cached.

## creates a list of accessor functions to the matrix provided as 
## an argument for this function and its inverse
## The inverse of a matrix is only calculated on demand by 'cacheSolve' function.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(.x) {
        x <<- .x
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(.inv) inv <<- .inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns a matrix that is the inverse of 'x'
## This function tries to obtain an inverse function
## that is cached inside of 'x' object first, and only
## if it's not available calculates and saves it inside
## 'x' object
cacheSolve <- function(x, ...) {
    ## try to obtain cached inverse matrix
    inv <- x$getinv()
    
    if (is.null(inv)) {
        m <- x$get()
        inv <- solve(m, ...)
        x$setinv(inv)
    } else {
        message("getting cached data")
    }
    
    inv
}
