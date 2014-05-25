## A pair of functions. First creates matrix object which can store it's own 
## cached inversion in order to avoid unecessary calculations. Second does the
## calculation (for first-run) or returns cache (for any next execution).

## This function creates a special "matrix" object that can cache its own
## inversed value.
makeCacheMatrix <- function(x = matrix()) {
        
        # Clear the cache.
        inverse <- NULL
        
        # Sub-function to set an original matrix. This may be first time
        # matrix is provided or it can be existing matrix changed. 
        # Therefore clearing the cache.
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # Sub-function to return original matrix.
        get <- function() x
        
        # Sub-function to set the cache (inversion of original matrix).
        setinv <- function(inv) inverse <<- inv
        
        # Sub-function to return cached inversed matrix.
        getinv <- function() inverse
        
        # Return list of sub-functions available.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## Function returns a matrix which is inverse of matrix 'x'. When inversion 
## has already been calculated, cached value is returned straight away.
cacheSolve <- function(x, ...) {
        
        # Check if matrix object already contains cached inverted matrix.
        # If so, return it instantly.
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        # No cached result yet, calculation needs to be performed using
        # solve() function. Cache is set and inverted matrix is returned.
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
        
}
