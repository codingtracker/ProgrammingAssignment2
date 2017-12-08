## Input a matrix, functions compute its inversed matrix, at the same time
## if input matrix duplicate with previous matrix, the cache will return its
## value without wasting CPU cycle again.


## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <-function() x
        
        setsolve <- function(solve) m <<- solve
        
        getsolve <- function() m
        
        list(set = set, get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve:
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        m <- x$getsolve()
        if(!is.null()){
                message("getting cached data")
                return(m)
        }
        date <- x$get()
        
        ## Return a matrix that is the inverse of 'x'
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
