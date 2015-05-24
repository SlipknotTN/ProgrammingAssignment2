## Calculates the inverse of a matrix using cache


## Stores the matrix and the cached inverse. Exposes get and set functions.
## Returns a list of functions.
makeCacheMatrix <- function(x = matrix()) {
    #Sets inverse to null on init (inverse not yet calculated)
    i <- NULL
    #Updates the stored matrix and resets the inverse
    set <- function(y) {
        #Update matrix
        x <<- y
        #Reset cached inverse
        i <<- NULL
    }
    #Gets the matrix
    get <- function() x
    #Sets the inverse
    setinverse <- function(inverse) i <<- inverse
    #Gets the inverse
    getinverse <- function() i
    #List of available functions
    list(get=get, set = set,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x' (created with makeCacheMatrix).
## Gets the cached inverse or calculates the inverse if not available.
cacheSolve <- function(x, ...) {
    #Checks if the inverse is available
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        #Returns the cached inverse without a new calculation
        return(i)
    }
    #Calculates the inverse if not available and stores it in the cache
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    #Returns the inverse after calculation
    i
}
