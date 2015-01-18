## If a loop of the same matrix inversion is needed, these two functions
## will save the first calculation in the cache and use it in the next
## loops, efectively saving computing time.

## Takes a matrix to inverse as argument and outputs a list of four needed 
## functions for the cacheSolve function next.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	get <- function() {x}
	setinv <- function(inv) {m <<- inv}
	getinv <- function() {m}
	list(get = get, setinv = setinv, getinv = getinv)
}


## Takes the list outputted fron makeCacheMatrix, checks cache for inverse 
## matrix from previous calculation and if not found, calculates it and 
## outputs it.

cacheSolve <- function(x, ...) {
	m <- x$getinv() ##Gets the cached matrix
	if(!is.null(m)) {
		message("getting cached data")
		return(m) ##If a cache matrix exists, it is returned
	}
	matrix <- x$get()
	m <- solve(matrix,diag(1,dim(matrix)[1]), ...) ## solve(A,b) gives matrix X from b = A X
	x$setinv(m) ##Sets the inverted matrix to cache
	return(m)
}
## To use, set the cache matrix outside the loop and compute it inside.
## If not, the cache is reset to NULL each time and it is useless.
