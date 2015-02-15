## The following 2 functions are used to create an object to store cached original and inverted matrices


## makeCacheMatrix provides a list of 4 objects that are functions used to store(set) or retrieve(get) original and inverted matrices

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSOlve is a function that uses special cached matrix and solves it (finds the inverse). If the inverse has already been found and cached before
## the functions just retrieves the cached inverted matrix instead of solving it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i        
}
