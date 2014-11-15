## makeCacheMatrix and cacheSolve are a pair of functions to allow caching the
## inverse of a matrix.

## Example usage:
## Set the working directory to be the same of the cachematrix.R source code.
## > source("cachematrix.R")
## > normal_matrix <- matrix(c(4, 3, 3, 2), nrow = 2)
## > special_matrix <- makeCacheMatrix(normal_matrix)
## To retrieve the original matrix: special_matrix$get()
## To compute the inverse matrix, caching the result:
## > cacheSolve(special_matrix)
## In other words, the first time the inverse matrix will be computed, while 
## on subsequent attempts, a cached result will be returned instead.
## When using the cached result, the message "getting cached data" will inform
## you. In real production code the message should be removed.


## Creates an object containing a matrix, that is able to cache its inverse

makeCacheMatrix <- function(matrix = matrix()) {

    inverse <- NULL

    set <- function(matrix_param) {
        matrix <<- matrix_param
        inverse <<- NULL
    }

    get <- function() matrix

    setInverse <- function(inverse_param) inverse <<- inverse_param

    getInverse <- function() inverse

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}

