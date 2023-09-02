## Put comments here that give an overall description of what your
## functions do

## Create a list with function set, get, setinv and getinv for a given matrix


makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) sol <<- solve
        getsolve <- function() sol
        list(
                set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve
        )
}


## Checks if there is an invers of the matrix cached if not returns it and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        sol <- x$getsolve()
        if (!is.null(sol)) {
                message("getting cached inverse matrix")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsolve(sol)
        sol
}