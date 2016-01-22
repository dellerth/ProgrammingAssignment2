## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit to caching the
## inverse of a matrix rather than compute it repeatedly.

## These two functions (makeCacheMatrix & cacheSolve) make and cache the inverse of a matrix.

## Computing the inverse of a square matrix is done with the solve function in R.
## If X is a square invertible matrix, then solve(X) returns its inverse.
## Assuming that the matrix supplied is always a square invertible matrix.

## makeCacheMatrix function creates a special "matrix" object that stores a matrix,
## cache's its inverse and returns a list containing functions that:
##	set the values of the matrix
##	get the values of the matrix
##	set the values of the inverted matrix
##	get the values of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y			## the <<- operator assigns a value to matrix object in an
		m <<- NULL		## environment that is different from the current environment.
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve function computes the inverse of the special "matrix" created and returned by
## makeCacheMatrix function.  cacheSolve first checks to see if the inverse has already been
## calculated (and the matrix has not changed).  If yes, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix and sets
## the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m	## Return a matrix that is the inverse of 'x'
}