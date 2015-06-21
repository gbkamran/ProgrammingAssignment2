## We create a special matrix object that can cache its inverse.
## For the sake of ease, we assume that the matrix is always invertible

## The following function would create that special matrix object

makeCacheMatrix <- function(x = matrix()) 
{
    #matrix_inverse will hold the inverse of matrix
    matrix_inverse <- NULL
    
    set <- function(y)
    {
        #set the x to y for the main functon makeCacheMatrix
        x <<- y
        #since we set x to y, the matrixInverse should be reset to NULL
        matrix_inverse <- NULL
    }
    
    # this function would simply return the matrix x
    get <- function() x 
    
    #the following function sets the matrix_inverse to given argument
    setInverse <- function(local_mat_inv) matrix_inverse <<- local_mat_inv
    
    # this method returns inverse
    getInverse <- function() matrix_inverse
    
    #This object is essentially a list of these functions
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## Define a method which will return the inverse of 'global' matrix x
## This method cacheSolve will return a cached inverse if available
## Otherwise it will calculate the inverse and return that

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat_inv <- x$getInverse()
    
    if (!is.null(mat_inv))
    {
        message("getting cached inverse")
        return(matrix_inverse)
    }
    
    #Otherwise, solve for inverse of matrix
    given_matrix <- x$get()
    resultant_inverse <- solve(given_matrix)
    x$setInverse(resultant_inverse)
    
    return(resultant_inverse)
}
