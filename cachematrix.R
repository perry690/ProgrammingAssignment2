## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        set <- function(new) {
        x <<- new
        cache <<- NULL        
        }
        get <- function() x
        setInverse <- function(inverse) cache <<- inverse
        getInverse <- function() cache
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cache <- x$getInverse()
        if(!is.null(cache)) {
                message("getting cached data")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data, ...)
        x$setInverse(cache)
        cache
        
}
