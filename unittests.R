## a unit test to verify my functions

test_cachedsolve <- function(){
    source('cachematrix.R')
    x<- makeCacheMatrix(x)
    x$set(matrix(rnorm(1:81),9,9))
    y <- cacheSolve(x)
    
    ## comparison code adapted from https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html
    if(is.matrix(x$getsolve()) && is.matrix(y) && dim(x$getsolve()) == dim(y) && all(x$getsolve() == y))
    {
        print("passed")
    }else{
        print("failed")
    }
}

