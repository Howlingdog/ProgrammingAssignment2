## Put comments here that give an overall description of what your
## functions do

## Create a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y)  {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list (set = set, get = get,
              setinverse = setinverse, 
              getinverse = getinverse)
}


## Compute the inverse of the matrix returned by makeCacheMatrix. If inverse has
##already been calculated, retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if (!is.null(s)){
                message ("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setinverse(s)
        s
        
}
