## These functions make it possible to store results off a solve function in
## order to be reused without multiple solve function calls

##
## makeCacheMatrix() returns a list object that stores the results and values of our matrix
## adapted from examples given by Roger Peng PhD, Jeff Leek Phd and Brian Caffo PhD
## from https://class.coursera.org/rprog-015/human_grading/view/courses/973496/assessments/3/submissions
## my comments!

makeCacheMatrix <- function(x = matrix()) {
    ## initialize
    m <- NULL
    
    ## define set, will store the matrix into the environment x
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get the stored matrix
    get <- function()
        x
    
    ## solve and store to environment variable m
    setsolve <- function(solve)
        m <<- solve
    
    ## return the solved matrix
    getsolve <- function()
        m
    #define the object fields
    list(
        set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve
    )
}

## solve the matrix and cache the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the solve of 'x'
    m <- x$getsolve()
    
    ## return the cached result if found
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # perform the solve
    data <- x$get()
    m <- solve(data, ...)
    
    ## cache the result
    x$setsolve(m)
    m
}
