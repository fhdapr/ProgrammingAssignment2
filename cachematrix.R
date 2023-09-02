## Put comments here that give an overall description of what your
## functions do

## Create a list with function set, get, setinv and getinv for a given matrix


makeCacheMatrix <- function(x = matrix()) {
        varSolve <- NULL
        set <- function(y) {
                x <<- y
                varSolve <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) varSolve <<- solve
        getsolve <- function() varSolve
        list(
                set = set,
                get = get,
                setsolve = setsolve,
                getsolve = getsolve
        )
}


## Checks if there is an invers of the matrix cached if not returns it and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        varSolve <- x$getsolve()
        if (!is.null(varSolve)) {
                message("getting cached inverse matrix")
                return(varSolve)
        }
        varData <- x$get()
        varSolve <- solve(varData, ...)
        x$setsolve(varSolve)
        varSolve
}