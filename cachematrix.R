## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather
## than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).
## Therefore, the following pair of functions that cache the inverse of a 


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
		
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        
		
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
		
}
