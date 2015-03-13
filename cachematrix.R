## Function makeCacheMatrix() takes a matrix and wraps it with special object (internally represented by 'list' class).
## That object provides functions to store and get inverse of wrapped matrix.
## Also it allows to replace wrapped matrix with another. In this case previously cached results will be deleted.
## 
## Given an object created by makeCacheMatrix(), cacheSolve() calculates inverse of wrapped matrix and stores it in cache.
## On second call cacheSolve() will return cached inverse.
##
## Validation of arguments and parameter-dependent caching are omitted for simplification. It's not required by assignment.
## If you want (not for evaluation or grading purpose), you may look for more smart version on my previous github commit.

## This function creates a wrapper over a matrix, that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {   
    inverseCache <- NULL
    
    set <- function(y) {
        x <<- y
        inverseCache <<- NULL    
    }
    
    get <- function() {
        x
    }
    
    setInverseCache <- function(inverse) {
        inverseCache <<- inverse
    }
    
    getInverseCache <- function() {
        inverseCache
    }
    
    result <- list(
        set = set,
        get = get,
        setInverseCache = setInverseCache,
        getInverseCache = getInverseCache
    )
    
    result
}

## This function calculates inverse of matrix, using cache methods of object provided by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    result <- x$getInverseCache()

    if(!is.null(result)) {
        message("getting cached data")
        return(result)
    }

    data <- x$get()

    result <- solve(data, ...)
    x$setInverseCache(result)
    result
}