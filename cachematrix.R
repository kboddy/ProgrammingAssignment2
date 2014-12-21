## Cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
## Input matrix (assumed square and invertible).
## Return a list of functions that:
### 1. sets the matrix
### 2. gets the matrix
### 3. sets the inverse matrix
### 4. gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # initialize inverse to be null
    set <- function(y) {
        x <<- y # set new matrix
        inv <<- NULL # delete any previous cache of inverse (of old matrix)
    }
    get <- function() x # get matrix
    setinv <- function(inverse) inv <<- inverse # set inverse matrix
    getinv <- function() inv # get inverse matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## This function computes (if necessary) and caches the inverse of a matrix.
## Input a makeCacheMatrix object.
## Return the inverse of the matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinv() # get inverse matrix
    ## if inverse exists in cache, return it
    if(!is.null(inv)) {
        message("getting cahced inverse matrix")
        return(inv)
    }
    ## if inverse is not in cache, calculate inverse and cache it
    data <- x$get() # get matrix
    inv <- solve(data) # solve for inverse
    x$setinv(inv) # cache inverse
    inv
}
