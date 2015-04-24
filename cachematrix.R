## The following functions speed up to compute repeated matrix inversion 
## for a single matrix using caching technique 


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
            im <- NULL                                      ## im for inversion matrix
  
            set <- function(y) {                            ## set the matrix 
                  x <<- y
                  im <<- NULL
            }
            get <- function() x                             ## get the matrix
            setinv <- function(solve) im <<- solve          ## set inversion matrix
            getinv <- function() im                         ## get inversion matrix
            
            list(set = set,                                 ## store the functions
                 get = get,
                 setinv = setinv,
                 getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        im <- x$getinv()                                     ## get inversion from cache
  
        if(!is.null(im)){                                    ## check if it is not NULL
                  message("getting cached data")
                  return(im)                                 ## return the cache data and save some computation   
        } 
        data <- x$get()                                      ## if we are here we need compute
        im <- solve(data, ...)                               ## compute the inverse
        x$setinv(im)                                         ## save it to cache
        im                                                   ## return the computed inverse matrix
}
