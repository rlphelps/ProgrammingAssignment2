##   makeCacheMatrix:  Takes a matrix as a parameter and returns
##                     a list of functions (methods) and the
##                     original matrix
##
##   cacheSolve:  Takes the list created by makeCacheMatrix function
##                and checks to see if the matrix inverse exists i.e.,
##                has the "computation expensive" inverse already
##                been created. If so it returns the inverse of the
##                original matrix, otherwise it creates the inverse
##                using the solve() function and then caches the inverse

## Input a matrix and return a list object for use by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Take list object and return inverse if cached else invert and cache
## then return matrix inverse

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("Getting cached inverted matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
