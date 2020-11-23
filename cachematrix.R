## The two functions are used to calculate the inverse of a matrix and store to
## or retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
            invrs <- NULL
            set <- function(y) {
                    x <<- y
                    invrs <<- NULL
            }
            get <- function() {x}
            setInverse <- function(inverse) {invrs <<- inverse}
            getInverse <- function() {invrs}
            list(set = set, get = get, setInverse = setInverse, 
                 getInverse = getInverse)
                
}


## This function computes the inverse of the special "matrix" created above
## or retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## This returns a matrix that is the inverse of 'x'
    invrs <- x$getInverse()
    if(!is.null(invrs)){
        message("getting cached data")
        return(invrs)
    }
    dat <- x$get()
    invrs <- solve(dat, ...)
    x$setInverse(invrs)
    invrs
}

