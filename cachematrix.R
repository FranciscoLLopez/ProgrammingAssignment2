## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it repeatedly

## The first function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverse) i <<- inverse
    getInverseMatrix <- function() i
    list (set = set, get = get,
          setInverseMatrix = setInverseMatrix,
          getInverseMatrix = getInverseMatrix)

}


## The following function calculates the inverse of the matrix created with the 
## above function. First checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverseMatrix()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setInverse(i)
    i
}
