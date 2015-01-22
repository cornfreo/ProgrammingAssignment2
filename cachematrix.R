## The two functions in this file enable the creation of a square,
## invertible matrix and the caching of its inverse.
## 

## This function, createCacheMatrix, creates a special matrix
## with a list of functions for manipulating this matrix by
##        - setting the inverse of the matrix
##        - getting the inverse of the matrix
##        - setting the value of the input matrix in the cache
##        - getting the value of the input matrix from the cache

makeCacheMatrix <- function(x = matrix()) {
 
    matinv <- NULL
    set <- function(y) {
      cachedx <<- y
      matinv <<- NULL
    }
    get <- function() cachedx
    setinverse <- function(inverse) matinverse <<- inverse
    getinverse <- function() matinverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  
}


## This function, cacheSolve, gets its input matrix and 
##        - checks the input matrix against the cache
##        - if it matches the cached matrix it then
##              - retrieves the cached inverse of the input matrix
##              - if the retrieved inverse is NULL it recomputes the 
##                  inverse and sets the cached inverse to this value and 
##                  returns this calculated inverse
##              - if the retrieved inverse is not NULL it returns it
##        - if the input matrix and cached matrix do not match then
##              - it computes and caches the inverse and returns it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    data <- x$get()
    if (!is.null(data) )
    if (!is.null(inv) & data == x){
      message("here is the cached inverse")
      return( inv )
    }
    
    data <- x$get()
}

