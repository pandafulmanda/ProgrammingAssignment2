## makeCacheMatrix and cacheSolve work together to return the inverse of a matrix.
## If the inverse has been previously calculated, the inverse has been cached and
## will be returned without being recalculated.
##
## Example:
##
##   x_mat_cached <- makeCacheMatrix(x_mat)
##   x_inv <- cacheSolve(x_mat_cached)
##
## where
##   x_mat is an invertible matrix
##   x_mat_cached is a list of functions set, get, setinverse, and getinverse
##   x_inv will be the inverse matrix of x_mat
##


## makeCacheMatrix
##
## This function with take the matrix x and return a list of functions.
##
## Example:
##
##   x_mat_cached <- makeCacheMatrix(x_mat)
##
##   Then, you can use the following functions:
##     x_mat_cached$set, x_mat_cached$get, x_mat_cached$setinverse, and x_mat_cached$getinverse
## 
## The functions are:
##
## 1. set: sets the value of the matrix as passed in by the argument and creates a null placeholder for it's inverse
## 2. get: gets the matrix that was set
## 3. setinverse: sets the inverse of the matrix as passed in by the argument
## 4. getinverse: gets the inverse of the matrix as was set
##

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve
##
## This function requires the it's first argument to be the result of makeCacheMatrix.
##
## Example:
##
##   x_mat_cached <- makeCacheMatrix(x_mat)
##   x_inv <- cacheSolve(x_mat_cached)
##
## x_inv will be the inverse matrix of x_mat.
##
## Specifically, for any resulting cacheMatrix named x_mat_cached,
## cacheSolve will check first for if there is a cached inverse to return,
## before running solve on the original matrix to invert it.
##
## It does this by:
##
## 1. Getting the currently cached inverse by calling x_mat_cached$getinverse.
## 2. Checking whether the returned inverse value is null.  If it is not null,
##   1. Log a message
##   2. Return the cached value and exit this function.
##
## 3. Otherwise, it gets the actual matrix with x_mat_cached$get.
## 4. Solves for the inverse.
## 5. Caches the inverse using x_mat_cached$setinverse.
## 6. Returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
