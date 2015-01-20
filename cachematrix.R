####    cachematrix.R
####    This script codes a pair of functions to cache the inverse of a matrix
####            (ras, 01-20-2015)

##      The first function creates an object containing a function to:
##              A. Set the value of a matrix
##              B. Get the value of a matrix
##              C. Set the value of the inverse of that matrix
##              D. Get the value of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
        
}

##      The second function checks if the matrix inverse has been calculated
##              A. If yes, function returns the inverse from cache
##              B. If no,  function calculates the inverse


cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}