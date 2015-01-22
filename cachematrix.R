## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than computing it repeatedly. 
## The functions below implement functions that cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "matrix" object, a list of functions, to
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse matrix
##   4. get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The function below displays the cached inverse of a matrix, if available in cache. 
## Otherwise the inverse is computed using the function solve() and the result is cached.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}

# How to use:
# > x <- rbind(c(1,2), c(3,4))
# > mm <- makeCacheMatrix(x)
# 
# > mm$get()
# [,1] [,2]
# [1,]    1    2
# [2,]    3    4
# 
# Run without cached data:
# > cacheSolve(mm)
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# 
# Run again, now cached data is retrieved:
# > cacheSolve(mm)
# getting cached data
# [,1] [,2]
# [1,] -2.0  1.0
# [2,]  1.5 -0.5
# 

