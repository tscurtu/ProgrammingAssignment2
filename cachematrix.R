## Functions that enable the encapsulation of a matrix's inverse value

## Returns a wrapper over a matrix, consisting of a list of setters
## and getters for the matrix itself and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL
    set <- function(y) {
        x <<- y
        xi <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xi <<- inv
    getinv <- function() xi
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

## Takes an object returned by makeCacheMatrix() and returns the
## inverse of the encased matrix.
## If the object contains a cached inverse it it returned;
## if not, the inverse is computed and cached and then returned.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(is.null(inv)) {
        inv <- solve(x$get(), ...)
        x$setinv(inv)
        return(inv)
    }
    inv
}
