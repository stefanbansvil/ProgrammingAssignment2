## Caculates and caches the inverse of a matrix

## Creates a list of 4 functions

makeCacheMatrix <- function(x = matrix()) {
        #r esets the cache 
        i <- NULL
        # creates the function set() that creates and stores the matrix in the variable x  
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # retrieves the matrix
        get <- function() x
        # Saves cache
        setinv <- function(inverse) i <<- inverse
        # Gets cacge
        getinv <- function() i
        # Creates a list with the 4 functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Returns the inverse of the matrix either by retrieving the value from cache
## or calculating it if it has not been calculated before

cacheSolve <- function(x, ...) {
        # Uses the function getinv() to store the inverse in the variable i
        i <- x$getinv()
        # If the inverse has already been caculated, then i is not null and 
        ## the message is returned along with the value of i
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # If the inverse has not been caculated before,
        # the matrix is retrieved 
        mdata <- x$get()
        # and the inverse is calculated and store in i
        i <- solve(mdata,...)
        x$setinv(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
