## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a list which stores a matrix and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(inverse) inv <<- inverse
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## Write a short comment describing this function
## This function returns the cached inverse or
## calculates and stores the inverse if cache is empty
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("Getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
