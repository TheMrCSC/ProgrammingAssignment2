## Creating a new type of Matrix where the Inverse is Cached.
## Hence instead of computing the Inverse on the fly each time, we will be able 
## to just pick the cached value as required.

## makeCacheMatrix is used to create the cached matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cachesolve is used to solve for the inverse of the matrix.
## This will be done once, and the value will be saved in makeCacheMatrix object
## When called the second time it will just give the cached value.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
