## The two functions, when combined, allow the program to retrieve a complex
## calculation from cache instead of recalulating and tying up resources.
## Matrices can be quite complex, and inverting them could take too many resources.

## This function establishes a matrix, as well as the conditions to retrieve 
## values in the parent environment for future coding. If the matrix has been
## changed, it will set m to null, so that subsequent formulas do not retrive
## incorrect cached data.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(solve) m <<- solve
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function first tests whether a cache of data has been established.
## If not, it inverts the matrix and stores the value in m.
## If so, it retrieves the cached data.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}
