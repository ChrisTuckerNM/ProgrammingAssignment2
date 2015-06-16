## These functions make it possible to store results off a solve function in
## order to be reused without multiple solve function calls

##
## makeCacheMatrix() returns a list object that stores the results and values of our matrix
## adapted from examples given by Roger Peng PhD, Jeff Leek Phd and Brian Caffo PhD
## from https://class.coursera.org/rprog-015/human_grading/view/courses/973496/assessments/3/submissions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setsolve <- function(solve)
        m <<- solve
    getsolve <- function()
        m
    list(
        set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the solve of 'x'
    m <- x$getsolve()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
