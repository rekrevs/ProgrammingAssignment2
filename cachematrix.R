## Cacheing the Inverse of a Matrix
## The following two functions support cacheing the
## inverse of a matrix to avoid costly recomputation.

## makeCacheMatrix creates a special matrix object which
## can also cache a value. The cached value will be computed
## as a function of the matrix and retrieved instead of recomputing.
## Here this will be the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    get <- function() x
    setcache <- function(c) cache <<- c
    getcache <- function() cache
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}

## cacheSolve returns the inverse of the given special
## matrix object. This value will be computed and stored in
## the cache the first time cacheSolve is called on
## a matrix. On later calls the cached value will be retrieved
## and returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getcache()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setcache(m)
    m
}
