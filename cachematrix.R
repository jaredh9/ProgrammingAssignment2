##This file contains two functions to calculate/cache/retrieve the inverse of a matrix,
##thereby enabling a more efficient program by bypassing unnecessary and costly matrix inversions.

##This function (makeCacheMatrix) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    	set <- function(y){
        	x <<- y
        	inv <<- NULL
    	}
    	get <- function() x
    	setInv <- function(inverse_m) inv <<- inverse_m
    	getInv <- function() inv
    
    	list(set = set, get = get,
             setInv = setInv, 
             getInv = getInv)
}


##This function (cacheSolve) computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed),
##then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getInv()
    	if(!is.null(inv)){
        	message("getting cached matrix inverse data")
        	return(inv)
    	}
    	data <- x$get()
    	inv <- solve(data, ...)
    	x$setInv(inv)
    	inv
}
