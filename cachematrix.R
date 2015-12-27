# Matrix inversion is usually a costly computation and 
# there may be some benefit to caching the inverse of a 
# matrix rather than compute it repeatedly.
# makeCacheMatrix is a function that returns a list of functions
# Its purpose is to store a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:

# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
        
        # holds the cached value or NULL if nothing is cached
        inver <- NULL
        
        # store a matrix
        setMat <- function(a) {
                x <<- a
                inver <<- NULL
        }
        
        # returns the stored matrix
        getMat <- function() {
                x
        }
        
        # cache the given argument 
        setInv <- function(solve) {
                inver <<- solve
        }
        
        # get the cached value
        getInv <- function() {
                inver
        }
        
        # return a list
        list(setMat = setMat, getMat = getMat, 
             setInv = setInv, getInv = getInv)
}


# This function returns the inverse of the matrix. 
#It checks if the inverse is already calculated and if so then 
#it gets the result and skips the computation
# else sets the value in the cache via setinverse function.
# Calculates the inverse of "special" matrix created with
# makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInv()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- y$getMat()
        inverse <- solve(data)
        y$setInv(inverse)
        
        # return the inverse
        inverse
}
