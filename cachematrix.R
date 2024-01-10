## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	## initialize the inverse property
	inv <- NULL
	
	## set the matrix
	set <- function(matrix){
		x <<- matrix
		inv <<- NULL
	}
	
	## get the matrix
	get <- function() x
	
	## set the inverse of the matrix
	setInverse <- function(inverse) inv <<- inverse
	
	## get the inverse of the matrix
	getInverse <- function() inv
	
	## return a list of the methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## return the inverse if it's already set
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        
        ## get matrix from previous function
        mat <- x$get()
        
        ## calculate the inverse using matrix multiplication
        inv <- solve(mat,...)
        
        ## set inverse to the object
        x$setInverse(inv)
        
        ## return the matrix
        inv
}
