##########################################################
# Calculate matrix inverse using caching
##########################################################

#---------------------------------------------------------
# Create a special “vector”, which is really a list containing a functions:
#   set() the value of the matrix
#   get() the value of the matrix
#   setinv() the value of the inverse
#   getinv() the value of the inverse
makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    get <- function() mat
    setinv <- function(mat_inv) inv <<- mat_inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#---------------------------------------------------------
# Calculate inverse using special “vector” created with makeCacheMatrix()
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


#---------------------------------------------------------
# Test
# hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
# h8 <- hilbert(8); h8
# cache = makeCacheMatrix(h8)
# cacheSolve(cache)
# cacheSolve(cache)
# identical(cacheSolve(cache), solve(h8))
