## The R functions below are able to cache the potentially time-consuming computation of an inverse matrix. 
## Taking the inverse of a matrix is typically a very long computation, especially if it has to be computed repeatedly. 
## It makes more sense to cache the value of the inverse matrix so that when we need it again, 
## it can be looked up in the cache rather than recomputed.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {			# Initialize the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve 	# Utilize Solve function to compute inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()				# query the x matrix's cache
        if(!is.null(m)) {				# if there is a cache
                message("getting cached data")
                return(m)				# just return the cache, don't compute
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m 								#return the result
}

