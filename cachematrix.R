## Put comments here that give an overall description of what your
## functions do

## the two functions below works together to compute the inverse of a matrix
## the inverse of a matrix can be computed using the "solve" function
## this computation is usually a heavy costly computation 
## with the below functions we are trying to avoid such computation

########################################

## makeCacheMatrix:
## this function creates a special vector
## which is a list of length 4 containing 4 named elements
## each element is a function defined in the makeCacheMatrix environment
## the function uses the "<<-" operator to 
## define "x" & "m" the makeCacheMatrix environment, so we can access their values

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve:
## this function computes the inverse of the special matrix 
## returned by the makeCacheMatrix function
## if the inverse was already computed the function "retrieve"
## the inverse, otherwise it first gets the value of the matrix
## second computes the inverse using the "solve" function
## third sets the inverse of the matrix for the makeCacheMatrix object
## and finally returns the value of the inverse "m"

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        ## Return a matrix that is the inverse of 'x'
        m
}