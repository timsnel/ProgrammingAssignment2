## The two function below can calculated the inverse of a matrix. 
## In order to avoid unnecassary computation it it checked wether the inverse of the matrix is already
## computated. If it is it is returned if not is it calculated

## the function "makeCacheMatrix is a list of four functions that creates a special "matrix" object that can cache its inverse.
## The function 1: "get" simply returns the input value of the original function
## the function 2: "set" changes the value of m in original function  environment by using the "<<-" operator
## the function 3: "getsolve" returns the "m" value. No input required
## the function 4: "setsolve" assigns a value to m. It does not actually calculate the inverse matrix

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

## the function "cacheSolve" returns a matrix that is the inverse of 'x'
## first it checks if m is already assigned. 
## if it is, it simple returns it from cache.
## if not the inverse matrix is calculated and returned

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
