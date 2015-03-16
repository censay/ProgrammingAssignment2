## Creates two functions that act on square matricies
## The purpose is to save computation time by caching 
## the inverse of a matrix the first time it is 
## calculated.

## makeCacheMatrix() allows caching and setting
## and retrieving of the cache, it returns a list
## of four functions
makeCacheMatrix <- function(x = matrix()) {
    ## i is the inverted matrix, defaults to NULL
    i <- NULL
    
    ## sets the cached matrix (to be inverted)
    ## and clears the inverse when a new matrix
    ## is loaded
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## retrieves the value of the matrix
    get <- function() x
    
    ## function to save the inverted matrix
    setinverse <- function(inverse) i <<- inverse

    ## retrieves the value of saved inverted matrix
    getinverse <- function() i
    
    ## returns a list of functions
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse =getinverse)
}


## cacheSolve() uses makeCacheMatrix() to find out 
## if an inverse matrix has already been solved
## if so it returns that cached matrix.  If not, it
## calculates the inverse, and saves it.
cacheSolve <- function(x, ...) {
    ## Retreive a stored matrix
    i <- x$getinverse()

    ## If inverse matrix exists, return it and exit    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }

    ## The inverse matrix does not exist if this code
    ## is reached.  Retrieve the matrix to invert
    ## and then store it and return the value
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}