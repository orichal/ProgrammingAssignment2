## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix creates an object representing a matrix and optionally its cached inverse.
# The namespace of makeCacheMatrix contains:
# Variables:
#     x - the matrix
#     inv - the cached inverse
# Functions:
#     get()  - return the matrix data
#     set(mat) - sets the matrix data to the data contained in 'mat' parameter
#     getinv() - returns the cached inverse as-is. If nothing is cached then return NULL
#     setinv(inv) - sets the cached inverse data to be the data contained in 'inv' parameter

## Write a short comment describing this function
# makeCacheMatrix() is the object constructor. 'x' parameter is an initialization data.
# Parameters:
# 'x' - an optional initialization matrix: 
#     if 'x' is not provided matrix data is initialized to an empty matrix
# The chached inverse 'inv' is initialized to NULL

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL;
    # set the matrix data and initialize the cached inverse to NULL
    set <- function(mat) {
        x <<- mat
        inv <<- NULL
    }
    # get the matrix data
    get <- function() x
    # cache and inverse
    setinv <- function(i) inv <<- i
    # get the cached inverse
    getinv <- function() inv
    # return the list of functions that can manipulate the object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
# cacheSolve(x) is a function that received a cache-matrix object.
# if the cache-matrix has a cached inverse then it will be returned
# else, the inverse will be computed, cached into the cache-matrix object and returned
# Parameters:
#     'x' - a cache-matrix object

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    # if inverse is cached then return it
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    # else compute the inverse, cache it and return it
    mat <- x$get()
    inv <- solve(mat)
    x$setinv(inv)
    inv
}
