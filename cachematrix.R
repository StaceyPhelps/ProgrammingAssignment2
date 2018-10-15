## Coursera R Programming Week 3 Programming Assignment #2
## These functions cache the inverse of a matrix
## 
## This function produces a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
##  This function set the matrix value
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
##  This function gets the matrix value
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function returns the matrix that is the inverse of 'x' , the makeCacheMatrix
## If the inverse has already been calculated (and the 
## matrix has not changed), the cached inverse is returned. 

cacheSolve <- function(x, ...) {
       
        inv <- x$getInverse()
        if (!is.null(inv)) {
                         message("getting cached data")
                return(inv)
        }
        mtx <- x$get()
        inv <- solve(mtx, ...)
        x$setInverse(inv)
        inv
}