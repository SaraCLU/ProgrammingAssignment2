makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        getMatrix <- function() x                              
        setInverse <- function(inverse) invMatrix <<- inverse  
        getInverse <- function() invMatrix                     
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
}
## The following function calculates the inverse of matrix using matrix created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
                message("Getting Cached Invertible Matrix")   #Type message: Getting Cached Invertible Matrix 
                return(invMatrix)                             #return the invertible matrix
        }
        
        #if value of the invertible matrix is NULL then  
        MatrixData <- x$getMatrix()                     #get the original Matrix Data 
        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$setInverse(invMatrix)                         #set the invertible matrix 
        return(invMatrix)                               #return the invertible matrix
}
