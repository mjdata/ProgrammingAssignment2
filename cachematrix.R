## The following two functions are used to create a special object that stores
## a matrix and caches its inverse. By taking advantage of the Lexical scoping
## rules of R language, combined with the fact that functions can be returned
## as values from other functions, cashing the inverse of a matrix can avoid
## repeadetely processing the usually costly computation. These functions
## assume that the matrix supplied is always invertible and use <<- operator to
## assign a value to an object in an environment that is different from the
## current environment to preserve state inside of an R object.
##
## The function makeCacheMatrix creates a special "matrix" object, which is
## really a list of functions to
##      1. set the value of the matrix
##      2. get the value of the matrix
##      3. set the inverse of the matrix
##      4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setm <- function(y){
                x <<- y
                inv <<- NULL
        }
        getm <- function() x
        setinvs <- function(inverse) inv <<- inverse
        getinvs <- function() inv
        list(setm = setm, getm = getm,
             setinvs = setinvs, getinvs = getinvs)
}

## The function cacheSolve computes the inverse of the special "matrix" returned
## by the function makeCacheMatrix. If the inverse has already been calculated
## (and if the matrix has not changed), then cacheSolve retrieves the inverse
## from the cash and skips the computation. Otherwise, it calculates the inverse
## of the matrix and sets the value of the inverse in the cache via the setinvs
## function defined in makeCacheMatrix, and then returns the inverse of the
## matrix. If x is a square invertible matrix, solve(x) returns its inverse.

cacheSolve <- function(x, ...) {
        ## return a matrix that is a inverse of the supplied matrix
        inv <- x$getinvs()
        if(!is.null(inv)){
                message("caching from data")
                return(inv)
        }
        m <- x$getm()
        inv <- solve(m)
        x$setinvs(inv)
        inv
}
## Sample test:
## > x <- rbind(c(1, -1/4), c(-1/3,2))
## > list <- makeCacheMatrix(x)
## > cacheSolve(list)
## > list$getm() %*% cacheSolve(list)
