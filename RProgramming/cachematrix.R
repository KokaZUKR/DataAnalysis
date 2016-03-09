## load a MASS library. We will use ginv() function to inverse a matrix
library(MASS)

## The first function creates a special object(list), that contains:
# - matrix (and a method "get" in order to get it)
# - method getinverse(it returns an inverse of a matrix if it is not empty)
# - method setinverse(calculates an inverse of a matrix)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        get <- function() x
        setinverse <- function(ginv) m <<- ginv(x)
        getinverse <- function() m
        list(get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The second function returns an inverse of a given x:
# - x - special object created by makeCacheMatrix function
# - it checks if the inverse has already been calculated
# - if the inverse is NULL, it uses a method "setinverse" of an object x

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting cached inversed matrix")
                return(m)
        }
        data <- x$get()
        m <- ginv(data)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}