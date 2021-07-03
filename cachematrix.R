## Put comments here that give an overall description of what your
## functions do

## The two functions of the script are makeCacheMatrix and cacheSolve.
## makeCacheMatrix creates a special "matrix" object.
## makeCacheMatrix consists of set, get, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x<<- y
		inv<<- NULL
		}
	get <- function() {x}
	setInverse <-function(inverse) {inv <<- inverse}
	getInverse <- function() {inv}
	list(set = set, get = get, 
	     setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is used to solve for the inverse of the makeCacheMatrix function, or gets the inverse from the cache if it is already solved.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	mat <-x$get()
	inv <-solve(mat, ...)
	x$setInverse(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
