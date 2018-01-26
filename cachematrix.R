## makeCacheMatrix 
# This function represents a complex object of a matrix where the inverse could be stored
# it contains the followings function:
# set the matrix
# get the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve 
# This function gives the stored calculated inverse of the matrix
# the inverse could be already calculated

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    #if the inverse has already been calculated
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    #else the inverse is calculated 
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
