## These functions calculates inverse of an matrix and caches the result

## Returns pseudo matrix on which inverse calculation are applied
## The result of this function are actually list, so don't be confused

makeCacheMatrix <- function(matrix = matrix()) {
    inverseResult <- NULL
    
    # Matrix setter/getter
    setMatrix <- function(newMatrix) {
        matrix <<- newMatrix
        inverseResult <<- NULL
    }
    getMatrix <- function() matrix
    
    # Inverse setter/getter
    setInverse <- function(newInverse) inverseResult <<- newInverse
    getInverse <- function() inverseResult
    
    # Functions list
    list(
        setMatrix = setMatrix, 
        getMatrix = getMatrix,
        
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## Calculates inverse of matrix with caching result if matrix don't change
## Works only with matrix created via `makeCacheMatrix()`

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    
    matrix <- x$getMatrix()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    
    inverse
}
