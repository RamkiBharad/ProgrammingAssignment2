
## Following function creates a special matrix object along with cahce space
## to store inverse of that matrix. It has four sub-functions which either sets or gets
## matrix or inverse matrix depending on which function is used

makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
    setMatrix <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    getMatrix <- function() x
    setInvMatrix <- function(im) invMatrix <<- im
    getInvMatrix <- function() invMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

## Following function will take output of above function and retrieves inverse matrix
## from cache and prints it. If cache is empty then it inverses, stores in cache
## prints it

cacheSolve <- function(x, ...) {
    im <- x$getInvMatrix()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$getMatrix()
    im <- solve(data, ...)
    x$setInvMatrix(im)
    im
}
