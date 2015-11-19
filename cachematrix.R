## This function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <-  function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special matrix based on makeCacheMatrix. If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()		## To get the inverse of x
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
