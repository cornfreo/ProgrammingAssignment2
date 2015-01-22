## The two functions in this file enable the creation of a cache
## for the purpose of storing a square invertible matrix and its
## inverse such that the inverse is only ever calculated once for 
## a given matrix and can be retrieved any number of times.
## The purpose is to save time and computer capacity.

## This function, createCacheMatrix, creates a special matrix
## with a list of functions for manipulating this matrix by
##        - setting the inverse of the matrix in the cache
##        - getting the inverse of the matrix from the cache
##        - setting the value of the input matrix in the cache
##        - getting the value of the input matrix from the cache
## Note that the set() method nulls the inverse because it is required
## to be recalculated and cached.

makeCacheMatrix <- function(x = matrix()) {
 
    matinverse <<- NULL
    cachedx <<- NULL
    cachedx <<- x
##  The set(x) method whereby the matrix is cached and the cached inverse
##      is reset to NULL as the data has changed
    set <- function(y) {
      cachedx <<- y
      matinverse <<- NULL
    }
## The get() method which returns the cached matrix
    get <- function() cachedx
## The setinverse(x) method which caches an externally calculated inverse
##        of the cached matrix
    setinverse <- function(inverse) matinverse <<- inverse
## The getinverse() method which returns the cached value of the inverse
##       which can take on non-NULL or NULL values
    getinverse <- function() matinverse
## Return a list of methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  
}


## This function, cacheSolve, gets its input list function and 
##        - checks if a cached inverse is available (ie not NULL)
##              and returns it if it is available
##        - if it is not available (ie NULL) it calculates the inverse and
##              caches and returns it
cacheSolve <- function(x = list(), ...) {
        ## Return a matrix that is the inverse of 'x'
##    get what is currently cached for the inverse - NULL or valid inverse
      inv <- x$getinverse()
##    check if the cached value is not NULL 
      if (!is.null(inv)){
##        the inverse exists therefore return it
          message("here is the cached inverse")
          return( inv )
      }
## The inverse does not exist - get the matrix, invert, cache and return it      
      data <- x$get()
      inv <- solve(data)
      x$setinverse(inv)
      inv
}


