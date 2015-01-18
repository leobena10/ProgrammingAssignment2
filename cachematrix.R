## If a loop of the same matrix inversion is needed, these two functions will save the first 
## calculation in the cache and use it in the next loops, efectively saving computing time.


makeCacheMatrix <- function(x = matrix()) {

	## Takes a matrix to inverse as argument and outputs a list of four needed 
	## functions for the cacheSolve function next.
	
	m <- NULL

	get <- function() {x} ##Gets the matrix to inverse upon calling

	setinv <- function(inv) {m <<- inv} ##Sets the argument (inverted matrix) to the cache

	getinv <- function() {m} ##Upon calling, gets the cache matrix

	list(get = get, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {

	## Takes the list outputted fron makeCacheMatrix, checks cache for inverse 
	## matrix from previous calculation and if not found, calculates it and outputs it.

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
