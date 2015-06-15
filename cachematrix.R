# a pair of functions that compute the inverse of a matrix 
# unless the inverse has been computed and cached.

# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv_x <<- inv
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

# The second function, cacheSolve calculate the invere of the matrix 
# creatd with the function makeCacheMatrix.
# It first checks to see if the invere matrix has already been calculated.
# If so, it gets the inverse matrix from the cache and skips the computation.
# Otherwise, it calculates the inverse and sets the value in the cache
# via the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv_x <- x$getinv()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinv(inv_x)
        inv_x
}
