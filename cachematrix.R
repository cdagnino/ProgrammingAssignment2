## These two functions provide a way to Invert a matrix
## Since inverting can be costly, cacheSolve checks
## if the inverse of the matrix has already been calculated


## makeCacheMatrix creates a special list the contains a function
#to set/get the value of the matrix and set/get the inverse
#of the matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Takes a matrix as input
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of a matrix.
# If the inverse has already been calculated, cacheSolve
# should retrieve the inverse from the cache
# The input must be a special CacheMatrix obtained
# from applying makeCacheMatrix to a regular Matrix first

cacheSolve <- function(x, ...) {
    # Returns a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv  <- solve(data, ...)
    x$setinverse(inv)
    inv
}
