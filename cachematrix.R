## The two functions below are used to compute, cache, and return the inverse of a square matrix. In the case that the inverse of a matrix has already been computed and cached, the cached inverse will be returned and will not be recomputed.

## Function 'makeCacheMatrix' creates a special matrix object that can cache its inverse. It contains functions to set and get the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
     ## Function to cache and return matrices
     
     i <- NULL                                    ## initialize i, where inverse will be cached
     
     set <- function(y) {                         ## used if you want to change the matrix
         x <<- y                                  ## y (changed matrix) replaces x
         i <<- NULL                               ## i is reset
     }
     
     get <- function() x                          ## returns cached matrix
     
     setinverse <- function(newinv) i <<- newinv  ## caches inverse matrix (sets value of i to new inv)
     
     getinverse <- function() i                   ## returns cached inverse matrix
     
     list(set = set,                              ## list of four functions is returned
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Function 'cacheSolve' calculates the inverse of the special matrix object created with function 'makeCacheMatrix'. It first checks to see if the inverse has already been calculated. If it has, it gets the inverse from the cache and skips the computation; if it has not, it calculates the inverse and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
     ## Function to get cached inverses or compute new ones
     
     i <- x$getinverse()                          ## i is cached inverse matrix
     
     if(!is.null(i)) {                            ## if value of i is not NULL,
          message("getting cached inverse")       ## message is displayed and
          return(i)                               ## value of i is returned
     }
                                                  ## if value of i is NULL,
     mat <- x$get()                               ## matrix cached by 'makeCacheMatrix' is assigned to mat,
     i <- solve(mat, ...)                         ## inverse is computed and assigned to i,
     x$setinverse(i)                              ## i is cached in 'setinverse' and
     i                                            ## i is returned
}
