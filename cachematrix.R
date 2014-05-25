## A pair of functions. First creates matrix object which can store it's own 
## cached inversion in order to avoid unecessary calculations. Second does the
## calculation (for first-run) or returns cache (for any next execution).

## This function creates a special "matrix" object that can cache its own
## inversed value.
makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get <- function() x
        setinv <- function(inv) inverse <<- inv
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Function returns a matrix which is inverse of matrix 'x'. When inversion 
## has already been calculated, cached value is returned.
cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
        
}
