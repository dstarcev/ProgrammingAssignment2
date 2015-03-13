## Function makeCacheMatrix() takes a matrix and wraps it with special object (internally represented by 'list' class).
## That object provides functions to store and get inverse of wrapped matrix.
## Also it allows to replace wrapped matrix with another. In this case previously cached results will be deleted.
## 
## Given an object created by makeCacheMatrix(), cacheSolve() calculates inverse of wrapped matrix and stores it in cache.
## On second call it returns cached inverse.
## It only stores a last result of calculation and also a last set of additional arguments.
## If we call cacheSolve() with another set of additional arguments, it will calculate inverse again.
## If we call cacheSolve()  with same set of additional arguments, it will return cached inverse.
## If we call cacheSolve() with invalid (not product of makeCacheMatrix()) object,
## it will forward call directly to regular solve() function, and result will not be cached.

## This function creates a wrapper over a matrix, that allows to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {   
    if (!is.matrix(x)) {
        stop("Invalid argument. 'x' should be a matrix")
    }
    
    inverseCache <- NULL
    inverseCacheArguments <- NULL
    
    set <- function(y) {
        if (!is.matrix(y)) {
            stop("Invalid argument. 'y' should be a matrix")
        }
        
        if (!identical(x, y)) {
            x <<- y
            inverseCache <<- NULL
            inverseCacheArguments <<- NULL
        }
    }
    
    get <- function() {
        x
    }
    
    setInverseCache <- function(inverse, arguments) {
        if (!is.matrix(inverse)) {
            stop("Invalid argument. 'inverse' should be a matrix")
        }
        
        inverseCache <<- inverse
        inverseCacheArguments <<- arguments
    }
    
    getInverseCache <- function(arguments) {
        if (identical(inverseCacheArguments, arguments)) {
            inverseCache
        }
        else {
            NULL
        }
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
    isCacheSupported <- function() {
        is.list(x) == TRUE && is.function(x$getInverseCache) == TRUE && is.function(x$setInverseCache) == TRUE
    }
    
    if (!isCacheSupported()) {
        warning("Unsupported type. Caching was skipped.")
        return(solve(x, ...))
    }
    
    arguments <- list(...)
    
    result <- x$getInverseCache(arguments)

    if(!is.null(result)) {
        message("getting cached data")
        return(result)
    }

    data <- x$get()

    result <- solve(data, ...)
    x$setInverseCache(result, arguments)
    result
}