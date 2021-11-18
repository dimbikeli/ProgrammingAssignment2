#The two functions below will return the inverse of a square matrix,
#using lexical scoping to avoid recalculating the inverse of the same matrix if re-called.

#First function:
#makeCacheMatrix() creates an R object that 
#stores a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
        #x is assumed to be an invertible square matrix
        inv <- NULL
        set <- function(y) {
                #We use << to assign the value y to x, which is an
                #object in the parent environment.
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        #the function returns a list that contains 
        #the matrix and its inverse
}

#Second function:
#if a new matrix is given, cacheSolve() below will calculate its inverse
#otherwise, it will retrieve the inverse from the cache value 
#stored in makeCacheMatrix()

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        #the function retrieve the inverse from the cache 
        #if the same matrix is given
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        #the function calculates the inverse if new matrix is given.
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
