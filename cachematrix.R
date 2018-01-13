## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

# set            set the value of a matrix
# get            get the value of a matrix
# setInverse     set the cached inverse of the matrix
# getInverse     get the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        # if nothing cached initially, holds NULL
        cache <- NULL      
        # store a matrix
        set <- function(new) {
        x <<- new
        cache <<- NULL        
        }
        # return cached matrix
        get <- function() x
        # cache the given argument
        setInverse <- function(inverse) cache <<- inverse
        # get the chached value
        getInverse <- function() cache
        # return list
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # get cached value
        cache <- x$getInverse()
        # cached value is not null, return
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        # otherwise, return inverse of the matrix
        data <- x$get()
        cache <- solve(data, ...)
        x$setInverse(cache)
        # return the inverse
        cache
        
}
