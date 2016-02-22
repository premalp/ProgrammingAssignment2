makeCacheMatrix <- function(x=matrix()){
        i<- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) i <<- solve
        getInverseMatrix <- function() i
        list(set = set, get = get, 
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)			
}

cacheSolve <- function(x, ...) {
        i <- x$getInverseMatrix()
        if (!is.null(i)){
                message("getting Cached Inverse of the matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverseMatrix(i)
        i        
}