## Functions for finding inverse of a matrix and caching the results


## Returns a wrapper for a given matrix that supports caching of 
## matrix's inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() { 
                x
        }
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Finds an inverse of a matrix. Uses cached result if available,
## otherwise finds inverses and stores into cache. Matrix must be 
## passed using a wrapper (created by makeCacheMatrix)
cacheSolve <- function(x, ...) {
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



#Example:
m = makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3))
cacheSolve(m) # Finds an prints inevrse of m
cacheSolve(m) # Retrieves inverse from cache and prints it