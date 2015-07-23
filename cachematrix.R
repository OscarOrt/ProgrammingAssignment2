## These functions compute the inverse matrix of a square matrix and it sets
## the information in a special cache matrix, which could be used later

## This function makes a special cache of the inverse matrix. For this purpose it
## makes initially a new variable called i, This value is set as NULL, then it 
## runs fourth sub-function and give back a list of these. This sub-function are:
## First, to set the value of the matrix; second, # to get the value of the
## matrix; third, to set the value of the inverse matrix and fourth to get the
## value of the mean. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- mean
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function calls initially to the function x$getinverse and set the value 
## obtained in i, if it is not NULL (It has been previously calculated), it 
## returns this value and finish the function. On the other hand, if it has not
## been calculated before (i is NULL) calls to x$get setting this information in 
## data, then it calculates the inverse matrix, it sets this value into the 
## cache ## and it returns the inverse matrix. 

cachesolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}