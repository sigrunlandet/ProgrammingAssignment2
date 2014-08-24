## Two functions to cache the invert of a square invertable matrix. 
## Pass a matrix to the makeCacheMatrix function, and a list is returned.
## cacheSolve is used on the list to find the inverse of the original matrix.

## Takes a matrix and returns a list of operators on the matrix, including caching 
## and retrieving the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes a list of the type created by the previous function, 
## and returns the inverse of the stored matrix. If the inverse is cached, it returns
## without going through the rest of the function. 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv ## Returns a matrix that is the inverse of 'x'
}
