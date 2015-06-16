## Coursera's R programming course (rprog-015)
## Programming Assignment 2 (prepared by Daulet Moldabayev)
## The two functions below are designed to avoid recomputation of
## inverse matrices if they have been already computed.

## makeCacheMatrix() -- takes 'x' matrix as an input and creates a list object
## with 4 elements: 
##      set() - changes the value of 'x', resets the inverse of 'x' to 'NULL'
##      get() - returns the value of 'x'
##      setinv() - sets the inverse of 'x'
##      getinv() - returns the inverse of 'x'
makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(computed_inv) invx <<- computed_inv
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve() -- computes the inverse to a matrix 'x' of makeCacheMatrix type
## if the inverse of 'x' was computed, cacheSolve() returns it without computing
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data)
        x$setinv(invx)
        invx
}
