## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	## Sets initial matrix in special "matrix" object
	set <- function(y)
	{
		x <<- y
		inv <<- NULL
	}
	## Returns initial matrix from special "matrix" object
	get <- function() x
	## Caches inverted matrix in special "matrix" object
	setInvMatrix <- function(inverted) inv <<- inverted
	## Returns inverted matrix from special "matrix" object
	getInvMatrix <- function() inv
	## Creating a special "matrix" object
	list(get = get, set = set, getInvMatrix = getInvMatrix, setInvMatrix = setInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	## Gets cached value of inverted matrix from special "matrix" object
	inv <- x$getInvMatrix
	## Checks is inverted matrix had already calculated
	if(!is.null(inv))
	{
		## If inversed matrix is already calculated get it from cache
		message("Getting matrix!")
		return(inv)
	}
	## If inversed matrix is not calculated then get initial matrix and calculate it
	buffer <- x$get()
	inv <- solve(buffer, ...)
	x$setInvMatrix(inv)
	inv
}
