## In this assignment, I have created two functions, which will compute
## the matrix inverse and also caches the inverse of the matrix to reduce
## the computation cost.

## This function creates a list containing functions to get and set the
## value of the matrix, and also functions to get and set the value of the
## inverse of the matrix. (Caching the inverse of a matrix)

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
  	set <- function(y) {
          x <<- y
          i <<- NULL
  	}
  	get <- function() x
  	setinverse <- function(inverse) i <<- inverse
  	getinverse <- function() i
  	list(set = set,
		get = get,
       	setinverse = setinverse,
       	getinverse = getinverse)
}

## The function below, calculates the inverse of the matrix, but it first 
## checks whether it is already been calculated. If so, it will get the
## the inverse from the cache and saves the computation cost.

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
  	if (!is.null(i)) {
          message("getting cached data")
          return(i)
  	}
  	data <- x$get()
  	i <- solve(data, ...)
  	x$setinverse(i)
  	i
}