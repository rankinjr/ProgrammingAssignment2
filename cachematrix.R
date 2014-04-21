## The following functions allow for the caching of a matrix and its inverse


## makeCacheMatrix, when assigned to a matrix variable, will contain 4 functions
## for getting and setting the initial matrix and the inverted matrix
## The caching will act as private variables in the assigned object for both
## the original matrix and the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                      # initialize a variable scoped in makeCacheMatrix
        ## the set function changes the value that is intially passed in
        set <- function(y) {
                x <<- y                # reset the parameter after initial call
                m <<- NULL             # reset the inverted matrix after the call
        }
        # the get function returns the passed in (or changed) matrix variable
        get <- function() x            # this returns the private variable
        
        # the setinverse routine inverts the matrix and stores the result in the m cache
        setinverse <- function(solve) m <<- solve  # this caches m to the inverted matrix
        # the getinverse function returns the cached inverted matrix
        getinverse <- function() m     # this returns the stored inverted matrix
        
        # the makeCacheMatrix returns a list of functions for getting and 
        # setting the inverted matrix value
        list(set = set, get = get,     # creates a list of functions to operate on the private vars
             setinverse = setinverse,
             getinverse = getinverse)
}


# The cacheSolve function returns the inverse of a matrix, unless the 
# input parameter has already had its matrix inverted, in which case it
# returns the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        ## x is a list with functions that operate on private variable to the object
        m <- x$getinverse()     # get the stored inverted matrix
        if(!is.null(m)) {       # if this is not the first call
                message("getting cached data")
                return(m)       # just return the stored inverted matrix
        }
        data <- x$get()         # get returns the private original matrix
        m <- solve(data, ...)   # invert the matrix
        x$setinverse(m)         # cache the inverted matrix in the make object
        m                       # return the inverted matrix
}
