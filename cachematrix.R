## These functions can be used to calculate and cache the inverse
## of a matrix. This will reduce the computation costs associated
## with repeatedly calculating the inverse of the matrix.

## Creates a special "matrix" object that can cache the inverse of the matrix.
## x must be an invertible matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <- NULL
    } 
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates and caches the inverse matrix of a matrix, 
## or uses the cached inverse matrix if it already exists.
## x must be a CacheMatrix created by passing an invertible matrix
## to makeCacheMatrix.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}