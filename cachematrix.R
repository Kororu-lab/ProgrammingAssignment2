## Put comments here that give an overall description of what your
## functions do
## two functions makeChaheMatrix() and cacheSolve() will find out
## inverse-matrix of given data(matrix). if same calculation had
## already done, it would find out cached data and give it.


## Write a short comment describing this function
## makeCache()will get matrix and then transform itself to be able
##  to cache it

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y){
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(sol) s <<- sol
    getsolve <- function() s
    list(set = set, get = get, 
         setsolve = setsolve, getsolve = getsolve)
    
}


## Write a short comment describing this function
## Normally the function would solve the matrix and return an
## inversed-matrix. However if R can find out cached data it
## would print out a message and return the cached data instead.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)){
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s    ## Return a matrix that is the inverse of 'x' calculated
}
