## The pair of functions below are meant to return the inverse of the square matrix. 
## If the inverse has already got computed and the matrix has not changed, then the function returns the cached inverse matrix.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
		if (ncol(x) != nrow(x)){
			message("Matrix is not a square matrix...cannot proceed")
      return(x)
		}
		i<- NULL
		set <- function(y){
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setInverseMatrix <- function(inverse) i <<- inverse
		getInverseMatrix <- function(from_cacheSolve = FALSE) {
        if (is.null(i) && from_cacheSolve != TRUE){
            message("Please call function cacheSolve(Matrix_to_be_cached_and_inverted) after calling makeCacheMatrix or set function")
            return(NULL)
        } 
        i
		}
		list(set = set, get = get, 
	  	  	setInverseMatrix = setInverseMatrix,
	   		  getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		i <- x$getInverseMatrix(TRUE)
		if (!is.null(i)){
					message("getting Cached Inverse of the matrix")
					return(i)
		}
		data <- x$get()
		i <- solve(data, ...)
		x$setInverseMatrix(i)
		i
}
