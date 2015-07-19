## The following functions can be used to compute and store the inverse
## of a square matrix, so that the operation of inversion is not repeated
## if it has been done previously on the same matrix.

## makeCacheMatrix() creates matrix associated with a list of functions that are 
## used to get the matrix to invert, calculate the inverse matrix and return the 
## inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse.matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse.matrix <<- solve
    getinverse <- function() inverse.matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() takes as argument a matrix created by the makeCacheMatrix() function,
## computes and returns its inverse. If the inverse matrix has already been computed before,
## the computation is not repeated but the cached data are returned, along with a message.

cacheSolve <- function(x = matrix(), ...) {
    inverse.matrix <- x$getinverse()
    if(!is.null(inverse.matrix)) {
        message("getting cached data")
        return(inverse.matrix)
    }
    data <- x$get()
    inverse.matrix <- solve(data, ...)
    x$setinverse(inverse.matrix)
    inverse.matrix
    ## Return a matrix that is the inverse of 'x'
}
