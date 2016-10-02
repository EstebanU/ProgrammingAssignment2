# the following two functions essentially compute the inverse of an invertible
# matrix. If the inverse has already been computed, the computation is not done
# and the stored result is displayed

# the following function returns a list of functions that
# 1. gets the matrix called x
# 2. sets the inverse inv to the inverse of the matrix passed as an argument.
#    inv is a global variable 
# 3. retrieves the inverse


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    get <- function() 
        x
    
    setinverse <- function(inverse) 
        inv <<- inverse
    
    getinverse <- function() 
        inv
    
    list(get = get, setinverse = setinverse, getinverse = getinverse)
}

# the function cacheSolve calculates the inverse of an invertible matrix by
# using the solve function. If the inverse has the NULL value, then the inverse
# is computed and set using the setinverse function. If the inverse has already 
# been computed, it prints out a message displaying the stored result. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached matrix inverse")
        return(inv)
    }
    
    m <- x$get()
    inv <- solve(m, ...)
    x$setinverse(inv)
    inv
}
