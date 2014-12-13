## Programming Assignment 2 - Functions to create a special vector to allow cacheing of answers ( in this
## case to invert a matrix) and to retrieve the answer from cache if available or to calculate it.

## Creates the special "vector" with the list of functions to set and get value of matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Return a list of functions that set and get a matrix and its inverse
        mat_inv <- NULL
        set <- function(y){
                x <<- y
                mat_inv <<- NULL
        }
        get <- function () x
        setinv <- function (inv) mat_inv <<- inv
        getinv <- function () mat_inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Checks to see if solution is in cache, if not calculates inverse using solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinv()
        if(!is.null(mat_inv)){
                message("getting cached data")
                return (mat_inv)
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$setinv(mat_inv)
        mat_inv
        
}
