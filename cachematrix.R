## makeCacheMartix creates a list of functions that user can subsequently use for the 
## following respective purposes:
##   set     - sets data matrix in cache
##   get     - gets the data matrix that has been cached
##   setinv  - sets the inverse of the data matrix in cache
##   getinv  - gets the inverse of the data matrix that has been cached

makeCacheMatrix <- function(x = matrix()) {
        matrixinv <- NULL
        
        set  <- function (y) {
                x <<- y
                matrixinv <<- NULL
        }
        get <- function() x
        setinv <- function (mi) matrixinv <<- mi
        getinv <- function () matrixinv
        
        list ( set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve works in conjunction with makeCacheMatrix, it computes the inverse of the 
## special matrix object created by makeCacheMatrix.  A check to see if the inverse has 
## already been computed and cached, if so the cached result is retured, otherwise it 
## involves solve() to do the computation and sets the result in cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        matrix <- x$getinv()
        if (!is.null(matrix)) {
                message ("getting cached data matrix")
                return (matrix)
        }
        data <- x$get()
        matrix <- solve(data)
        x$setinv (matrix)
        matrix
}