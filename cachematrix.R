## Overall the function enable faster processing of data by means of caching. Here, a matrix
## is first initiated and caching is enabled. The initiation of the special matrix allows
## for functional arguments that can be used access or store a provided matrix.


## The function makeCacheMatrix initiates a special matrix which specifically makes 
## use of functions in order to cache and store the inverse of a provided matrix.

makeCacheMatrix <- function(x = matrix()) {
        # Set the default matrix to null
        m <- NULL
        # Initiate a set of functions that act on the provided matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverseMatrix) m <<- inverseMatrix
        getinverse <- function() m
        # Initiate a list that contains all relevant functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculated the inverse of a square matrix.
## However, the function also checks whether the inverse as been calcualted before,
## by checking the cache provided by makeCacheMatrix. If an inverse exists
## already in the cache, this stored matrix is returned instead of engaging in
## additional calculations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # Check whether the inverse has already been calculated
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        # If no inverse was stored, calculate the inverse and store it
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}
