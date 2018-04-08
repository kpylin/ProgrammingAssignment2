## This R function is able to cache potentially time-consuming 
## computations.  If the contents of a vector are not changing,
## it may make sense to cache the value of the mean so that
## when we need it again, it can be looked up in the cache
## rather than recomputed.

## makeCacheMatrix creates a special list "matrix" object that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve calculates the inverse of matrix that created with
## the above function.

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
        ## Return a matrix that is the inverse of 'x'
}

## TESTING WITH MATRIX OF NUMERIC
## > x <- matrix(c(1, 2, 3, 4), c(2, 2)) ## must be square
## > m <- makeCacheMatrix(x)
## > cacheSolve(m)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
