## This pair of functions cache the inverse of a matrix to avoid redundant calculations.
## `makeCacheMatrix` creates a special object that stores a matrix and caches its inverse.
## `cacheSolve` computes the inverse of the matrix, retrieving the cached result if available.

## makeCacheMatrix: 
## This function creates a special "matrix" object that can store its inverse.
## It provides methods to set and get the matrix, as well as set and get the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <<- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve:
## This function computes the inverse of the matrix stored in the special "matrix" object.
## If the inverse has already been computed and cached, it retrieves the cached result.
## Otherwise, it calculates the inverse, stores it in the cache, and returns it.

cacheSolve <- function(x, ...) {

    inv <- x$getInverse() 
    
    if(!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...) 
    x$setInverse(inv)
    
    inv
}