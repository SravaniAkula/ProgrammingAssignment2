## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that is used to create a special vector that contains following 4 functions.
## 1. To set value of vector.
## 2.get the value of vector
## 3.set value of inverse
## 4.get value of inverse

makeCacheMatrix <- function(x = matrix()) {
    invr <- NULL
    set <- function(y) {
        x <<- y
        invr <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invr <<- inverse
    getInverse <- function() invr
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cachesolve function calculates inverse of special vector created.
## It first checks if its already calculated in cache, if it is calculated already, itll return value without computing again.
## If not it'll compute the value and store it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invr <- x$getInverse()
    if(!is.null(invr)) {
        message("getting cached data")
        return(invr)
    }
    data <- x$get()
    invr <- solve(data, ...)
    invr
}