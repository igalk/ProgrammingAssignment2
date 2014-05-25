## This file includes functions for cached calculation of the inverse of a matrix.
## There will be 2 functions in this file. One will created a cached environment to store
## both the original matrix and it's inverse.
## The second function will use the first as a data store and will retrieve the cached
## value of the inversed matrix, if such a cache exists. Otherwise, it will calculate the
## inverse of the matrix and store for future calculations.

## For example, we know that the inverse of matrix(c(2,3,1,2), nrow=2, ncol=2)
## is matrix(c(2,-3,-1,2), nrow=2, ncol=2)
## So an example usage of those methods would look like this:
## > c <- makeCacheMatrix(matrix(c(2,3,1,2), nrow=2, ncol=2))
## > cacheSolve(c)
## [,1] [,2]
## [1,]    2   -1
## [2,]   -3    2
## > cacheSolve(c)
## getting cached inversed matrix
## [,1] [,2]
## [1,]    2   -1
## [2,]   -3    2

## This function is used as a data store. Once it gets the matrix as a parameter, it
## stores the matrix in an internal environment.
## It provides as output a list of functions: get, set, getInversedMatrix and setInversedMatrix
## get - retrieves the original matrix that is stored.
## getInversedMatrix - retrieves the stored inversed value of the matrix.
## set and setInversedMatrix store values accordingly.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInversedMatrix <- function(inverse) m <<- inverse
        getInversedMatrix <- function() m
        list(set = set, get = get,
             setInversedMatrix = setInversedMatrix,
             getInversedMatrix = getInversedMatrix)
}

## This function does the calculation. It gets an input the list we get as an output
## from makeCacheMatrix.
## It then looks for a cached value in the stored environment.
## If such a value is found, it is immediately returned without further calculations.
## Otherwise, the inverse of the matrix is calculated and store in the cache.
cacheSolve <- function(x, ...) {
        m <- x$getInversedMatrix()
        if(!is.null(m)) {
                message("getting cached inversed matrix")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setInversedMatrix(m)
        m
}
