## Put comments here that give an overall description of what your
## functions do

## These functions allow a matrix calculation (inverse of a matrix) 
## to be cached, which is to say inverse of the matrix has already 
## been calculated and the matrix has not changed then the function will
## return the cached matrix inverse rather than recalculate it.
##

## Write a short comment describing this function

## The makeCacheMatrix() function creates a special "Matrix"
## it is is actually a list.  The list contains functions which carry
## out the following tasks.  1. set the value of the matrix, 2. get the 
## vaule of the matrix, 3. set the vaule of the matrix inverse, 4. get
## the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get=get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" object that
## was created by the above function. If the inverse has already been
## calculated then the fundtion will retireve the inverse from the cache.
## The function lets you know it is returning a chached value with the 
## text "getting cached data"

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}