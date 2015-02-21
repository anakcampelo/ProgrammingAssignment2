## This function computes the inverse of the special "matrix" 'x' returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

## This function creates a special "matrix" object 'x' that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
       set <- function(y) {
                x <<- y
                s <<- NULL
        }
get <- function() x
        setsolve <- function(solve) s <<- solve
getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function returns a matrix that is the inverse of 'x' makeCacheMatrix above.

cacheSolve <- function(x, ...) {
s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
