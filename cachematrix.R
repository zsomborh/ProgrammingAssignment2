## The below code is used as submission for the R programming 
## coursera course where my task is to practice how to make use 
## of caches for computationaly heavy tasks
## 
## 
## I was tasked to introduce two functions both of which will 
## cache the inverse of a matrix
##
## 1) makeCacheMatrix will create a special matrix object with a list of four methods
##   
## get() - Gets the actual matrix
## set() - Sets the value of the acutal matrix
## getinverse() - Gets the inverse of matrix
## setinverse() - Sets the inverse of matrix


## Creates a cache object initialized with the passed matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(matrix_other) {
        x <<- matrix_other
        inverse <<- NULL
    }
    get <- function() x #returns the matric that we 
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## 2) cacheSolve will solves the inverse only if it hasn't been calculated before 

cacheSolve <- function(x, ...) {
    inverse <- x$getinv() 
    if (!is.null(inverse)) { 
        message("getting cached inverse")
        return(inverse)
    }
    xmatrix <- x$get()
    inverse <- solve(xmatrix)
    x$setinv(inverse)
    return(inverse)
}