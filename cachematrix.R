## The functions described in this file provide the inverse of a matrix
## and this result is cached for performance in case inverse of matrix is 
## requested to be computed again.
## Please note that the input matrix is assumed to be always invertible.

## The following function makeCacheMatrix takes input as a matrix which 
## is assumed to be invertible. It stored the input matrix in its environment
## and returns a list of functions which allow additional operations on the 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                #reset the inverse and matrix stored in the main function's env
                i <<- NULL
                x <<- y
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function () i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is the function responsible for getting the inverse of a matrix.
## This function takes the resultant object from the makeCacheMatrix 
## and checks if it already has an inverse cached in that object. If the
## cached inverted matrix is present then that is returned as solution 
## otherwise it calculates the inverse of the matrix and stores it in the 
## input object's cache and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i))
        {
            message("cached result found")
            return(i)
        }
        i=solve(x$get(),... )
        x$setinverse(i)
        i
}
