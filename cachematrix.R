## The "makeCacheMatrix" function creates an object of class matrix that 
#can cache its inverse. The "cacheSolve" function returns the inversed version of
#the matrix that has been returned by the aforemntioned function. If its inverse
#has been already computed then it returns the cached result.

## This is the function tha creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function checks if the inverse of the matrix that has been returned from
#the previous function, has been already computed. IF so, it returns the cached
#result. Otherwise, it computes its inverse and returns the 'fresh' result.

cacheSolve <- function(x, ...) {
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matr <- x$get()
        i <- solve(matr, ...)
        x$setinverse(i)
        i
}
