## makeCacheMatrix and chacheSolve are two functions used invert a matrix and cache the result
##
## test using:
## myMatrix <- matrix(rnorm(49), nrow = 7)
## cacheMatrix <- makeCacheMatrix(myMatrix)
## cacheMatrix$get()                            
## cacheSolve(cacheMatrix)
## cacheSolve(cacheMatrix)  

# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# chacheSolve calculates and caches the inverse of the special "matrix" created by makeCacheMatrix.
# if already calculated, then return cached inverse.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
