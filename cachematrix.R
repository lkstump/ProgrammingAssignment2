## Creates matrix object that caches the inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initializes null object called i
    i <- NULL

    ## Sets the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Gets the matrix
    get <- function() {
    	## Returns the matrix
    	m
    }

    ## Sets the inverse of the matrix
    setinverse <- function(inverse) {
        i <<- inverse
    }

    ## Gets the inverse of the matrix
    getinverse <- function() {
        ## Returns the inverse
        i
    }

    ## Returns list of methods
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {

    ## Returns matrix that is inverse of 'x'
    m <- x$getinverse()

    ## Returns inverse if already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Retrieves matrix from the object
    data <- x$get()

    ## Calculates the inverse
    m <- solve(data) %*% data

    ## Sets the inverse
    x$setinverse(m)

    ## Returns the matrix
    m
}