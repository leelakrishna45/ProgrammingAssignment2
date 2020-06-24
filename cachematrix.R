## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions
## that cache the inverse of a matrix.
 


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(matrix = matrix()) {
        invMatrixVal <- NULL
        set <- function(y){
              matrix <<- y 
              invMatrixVal <<- NULL
        }
        get <- function() matrix
        getInv <- function() invMatrixVal
        setInv <- function(mean) invMatrixVal <<- mean
        list(get = get, set = set, getInv = getInv, setInv = setInv)
}


## This function computes the  inverse of a  "matrix" created by makeCahceMatrix above. If the inverse has already been calculated(and the 
## matrix has not changed), then the cachesolve should return the inverse matrix from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMat <- x$getInv()
        if(!is.null(invMat)){
                return(invMat)
        }
        mat <- x$get()
        #From the assignment, we can assume the matrix is always invertible
        invMat <- solve(mat)
        x$setInv(invMat)
}
