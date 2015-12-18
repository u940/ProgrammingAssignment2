## Functions to create a 'CacheMatrix' object, which stores a matrix
## and its inverse once it is calculated for the first time.


## Create a CacheMatrix from 'matrix' and return
## a list of functions to set and get the matrix and its inverse.
makeCacheMatrix <- function(matrix = matrix()) {

    ## variable that stores the inverse matrix
    cached_inv <- NULL
    
    # Updates the matrix and clears the old cached inverse.
    set <- function(new_matrix) {
        matrix <<- new_matrix
        cached_inv <<- NULL
    }
    
    get <- function() matrix
    
    setinv <- function(inv) cached_inv <<- inv
    
    getinv <- function() cached_inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## Return the cached inverse of 'cacheMatrix' if present.
## If not present, calculate the inverse and store it in
## 'cacheMatrix' before returning it.
cacheSolve <- function(cacheMatrix, ...) {
    ## Return a matrix that is the inverse of 'cacheMatrix'

    cached_inv <- cacheMatrix$getinv()
    if(!is.null(cached_inv)) {
        message("getting cached data")
        return(cached_inv)
    }
    matrix <- cacheMatrix$get()
    cached_inv <- solve(matrix, ...)  ## calculate inverse of matrix
    cacheMatrix$setinv(cached_inv)
    cached_inv  
}
