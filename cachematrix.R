## This source files has functions that cache the inverse of a matrix.
## For these functions to work, it is assumed that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## initialize inverse as null (default)
    m <- NULL

    ## function to set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ## function to get the matrix
    get <- function() x

    ## function to set invered matrix
    setinv <- function(inv) m <<- inv

    ## function to get invered matrix
    getinv <- function() m

    ## return special "matrix", which is really a list containing a function
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the inverse is retrieved from the cache

cacheSolve <- function(x, ...) {
    
    ## Check if the matrix is already cached
    m <- x$getinv()
    if(!is.null(m)) {
        ## inverse is already cached
        message("returning cached matrix")
        return(m)
    }
    
    ## Perform inverse matrix
    data <- x$get()
    m <- solve(data, ...)
    ## cache inverse
    x$setinv(m)
    ## Return a matrix that is the inverse of 'x'
    m
}
