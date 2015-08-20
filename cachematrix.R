## These functions provide inverse matrix calculation a cache the result
## for increased performance


## makeCacheMatrix returns a list of functions for CacheSolve function
## to set or get the inverted cached matrix
makeCacheMatrix <- function(x = matrix()) {
	invr <- NULL

	# create the matrix
	set <- function(y) {
		x <<- y
		invr <<- NULL
	}
	
	# get matrix and invert
	get <- function() x
	setinverse <- function(inverse) invr <<- inverse
	getinverse <- function() invr
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## cacheSolve ratrieves the cached inverse of the matrix 
## stored using makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it is stored
cacheSolve <- function(x, ...) {

	# gets inverted matrix value from cache if exist
	invr <- x$getinverse()
	if(!is.null(invr)) {
		message("getting cached data.")
		return(invr)
	}
	
	# calculate iverted value if not cached and store in cache
	data <- x$get()
	invr <- solve(data)
	x$setinverse(invr)
	invr
}