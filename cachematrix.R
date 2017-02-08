#Create a special matrix object that can cache its inverse,
#which is really a list containing a function to:
#1. set the matrix
#2. get the matrix
#3. set the inverse
#4. get the inverse
#Use <<- to assign a value to an object in an environment that is different from the current environment
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

#Create a function that computes the inverse of the special matrix.
#Computing the inverse of a square matrix can be done with the solve function.
#If the inverse has already been calculated, 
#then the cacheSolve should retrieve the inverse from the cache,
#otherwise it should calculate the inverse (with the solve function).
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
