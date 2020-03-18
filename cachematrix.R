## These functions look to save time when returning an inverse of a matrix. 
##These functions check to see if an inverse has already been calculated and stored.
##If this has previously happened, then a cached version is returned, otherwise,
## a new inverse of the matrix is calculated

## makeCacheMatrix returns a list of functions involved in creating the cache for inverse 
## matricies.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the matrix.  It returns a cached version if one is found
## in makeCacheMatrix, otherwise it carries out the calculation within the function.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}