## These functions find the inverse of a square matrix, which is typically an
## operation that takes a long time and lots of memory. Hence, once the inverse 
## is solved once, the result is cached in memory, avoiding the need to re-run
## the memory intensive calculation next time.

## The first function sets the matrix in the form of a list of functions.

makeCacheMatrix <- function(x = matrix()) {
        f <- NULL
        set <- function(y){
                x <<- y
                f <<- NULL
        }
        get <- function() x
        setinv <- function(inv) f <<- inv
        getinv <- function() f
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This second function finds the inverse of the matrix using the solve() 
## function and cache this value. However, if the inverse is already found, it 
## will print the cached value.

cacheSolve <- function(x, ...) {
        f <- x$getinv()
        if(!is.null(f)){
                message("Getting the cached data")
                return(f)
        }
        data <- x$get()
        f <- solve(data, ...)
        x$setinv(f)
        f
}
