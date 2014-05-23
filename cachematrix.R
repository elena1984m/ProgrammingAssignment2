## These functions are able to create a special "matrix" object
## and calculate its inverse. If the inverse il already calculated
## and the matrix is not changed, the functions retrieve the
## inverse from the cache.

## makeCacheMatrix create a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y		## initializing special "matrix" object
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve ##calc. matrix inverse
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" object
## created in makeCacheMatrix and retrives the inverse from the cache
## only if this has been already calculated, and the matrix has not 
## changed.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {	##checking if it has been already calc.
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()		##calc. matrix inverse (if not already done)
        inv <- solve(data, ...)
        x$setinv(inv)
        inv			##print the inverse
}
