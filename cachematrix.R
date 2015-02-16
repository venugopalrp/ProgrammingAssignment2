## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##############
#The first function, makeVector creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set inverse of the matrix
#get inverese of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invMtrx <- NULL
        set <- function(y) {
                x <<- y
                invMtrx <<- NULL
        }
        get <- function() x
        setInverse <- function(I) invMtrx <<- I
        getInverse <- function() invMtrx
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
#The following function returns inverse of the matrix from the list passed in argument
#However, it first checks to see if the inverse already exists.
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse using solve function and sets the value of the 
#inverse in the cache.
cacheSolve <- function(x, ...) {
        imatrix <- x$getInverse()
        if(!is.null(imatrix)) {
                message("getting cached inverse matrix")
                return(imatrix)
        }
        origmatrix <- x$get()
        imatrix <- solve(origmatrix, ...)
        x$setInverse(imatrix)
        
        return(imatrix)
}
### Examples
#mt <- makeCacheMatrix(matrix(1:4, 2, 2))
#mt
#class(mt)
#class(mt$set)
#mt$get()  
#cacheSolve(mt)
#cacheSolve(mt)
