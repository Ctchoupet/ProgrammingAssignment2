## Matrix inversion can be potentially time consuming, functions below will allow to cache the computation in some cases. 
## MakeCacheMatrix create a list containing functions to perform several operations on a matrix as described below:

# 1. set function set the value of the matrix
# 2. get function get the value of the matrix
# 3. setinverse function set the value of the inverse of the matrix provided
# 4. getinverse function get the value of the inverse of the matrix provided

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
set <- function(y) {
    x <<- y
    inv << NULL
}

get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        

inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data1 <- x$get()
        inv <- mean(data1, ...)
        x$setinverse(inv)
        inv
}
