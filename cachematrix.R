## example found in https://class.coursera.org/rprog-015/forum/thread?thread_id=447
# > m <- matrix(c(-1, -2, 1, 1), 2,2)
# > x <- makeCacheMatrix(m)
# > inv <- cacheSolve(x)
# > inv <- cacheSolve(x) ## gives message and return cached matrix
# > inv
#      [,1] [,2]
# [1,]    1   -1
# [2,]    2   -1

## o <- makeCacheMatrix(mat) creates an special cache-able matrix object, "o",
## from the "mat" matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## invmat <- cacheSolve(o) produce the inverse matrix of mat, "invmat", from "o"

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <-x$get()
    # For this assignment, assume that the matrix supplied is always invertible.
    inv <- solve(data)
    x$setinv(inv)
    inv
}
