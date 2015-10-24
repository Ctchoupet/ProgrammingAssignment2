## Matrix inversion can be potentially time consuming, functions below will allow to cache the computation in some cases. 
## MakeCacheMatrix create a list containing functions to perform several operations on a matrix as described below:

# 1. set function set the value of the matrix
# 2. get function get the value of the matrix
# 3. setinverse function set the value of the inverse of the matrix provided
# 4. getinverse function get the value of the inverse of the matrix provided

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
       x <<- y
       inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve function calculate the inverse of the matrix creted in the makeCacheMatrix function. 
## It first checks to see if the invere has already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets the value of the inversse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data1 <- x$get()
        inv <- solve(data1)
        x$setinverse(inv)
        inv
}