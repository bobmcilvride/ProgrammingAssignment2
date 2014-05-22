## Overview:
##
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
##
## These functions enable caching of the inverse of a square invertible matrix.
##
## See descriptions of each function below for details.
##
##

## Function:  makeCacheMatrix(x), where x is a matrix
##
## This function creates a special "matrix" object that can cache
## its inverse.
## 
## Example:
##
## > x <- matrix(c(1,2,3,4), nrow=2, ncol=2)  # create a 2x2 matrix
## > x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
##
## > a <- makeCacheMatrix(x)   # cache the inverse of x
## > a$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get the matrix
    get <- function() x
    
    # set the inverse of a matrix
    setmatrix <- function(solve) m <<- solve
    
    # get the inverse of a matrix
    getmatrix <- function() m
    
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)  
}

## Function:  solveCache(x), where x is a matrix
##
## This function calculates the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.
##
## Example:
##
## > cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## > cacheSolve(a)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
cacheSolve <- function(x, ...) {
    
    # Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    
    # Get from cache if it already exists
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # create the inverse
    data <- x$get()
    m <- solve(data, ...)
    
    # cache the inverse
    x$setmatrix(m)
    
    # return the inverse
    m
}