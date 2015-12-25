## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.
## makeCacheMatrix is a function that returns a list of functions
# Its purpose is to store a martix and a cached value of the inverse of the 
## matrix. Contains the following functions:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y){
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)

}

## This function returns the inverse of the matrix. It checks if the inverse
## is already calculated and if so then it gets the result and skips the computation
## else sets the value in the cache via setinverse function.
## Calculates the inverse of "special" matrix created with

cacheSolve <- function(x, ...) {
        ## get the cached value
        inver <- x&getinverse()
        ##if a cached value exists return it
        if(!is.null(inver)) {
                message("getting cached data")
                return (inver)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- x$get
        inver <- solve(data, ...)
        x$setinverse(inver)
        # return the inverse
        inver
}
