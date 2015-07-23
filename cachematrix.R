##  Below fucntions calculates the inverse of a matrix which is a 
##  potentially time-consuming computation. Following 

## makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
    inversem <- NULL
    set <- function(y) {
        x <<- y
        inversem <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inversem <<- i
    getinverse <- function() inversem
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve calculates the matrix of the special "vector" created 
## with the above function. it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of
## the data and sets the value of the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversem <- x$getinverse()
    if(!is.null(inversem)) {
        message("getting cached data")
        return(inversem)
    }
    data <- x$get()
    inversem <- solve(data)
    x$setinverse(inversem)
    inversem
}
