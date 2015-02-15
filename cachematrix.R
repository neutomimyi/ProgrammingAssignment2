## The following 2 functions are used to create an object to store cached original and inverted matrices
## To use these 2 functions first source the file, then assign makeCacheMatrix to a new variable e.g. x <- makeCacheMatrix, 
## then cache the original matrix (e.g. x$set(m))
## then you can use cacheSolve(x) to get the inverted matrix. When called first time the original matrix gets solved and cached, on the next calls 
## the original matrix doesn't get solved but the cached inverse is returned. 


## makeCacheMatrix provides a list of 4 objects that are functions used to store(set) or retrieve(get) original and inverted matrices

makeCacheMatrix <- function(x = matrix()) {
        ## i keeps the inverted matrix. When a new list makeCacheMatrix is created, i is empty
        i <- NULL
        ##function set stores the original matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ##function get retrieves the original matrix
        get <- function() x
        
        ##function setinverse stores the inverted matrix in variable i
        setinverse <- function(inv) i <<- inv
        
        ##functino getinverse retrieves the inverted matrix from variable i
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
        ## If the inverse has already been cached, just return the cached version
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If there is no cached inverse, retrieve original matrix
        data <- x$get()
        
        ## Solve the original matrix (get the inverted matrix)
        i <- solve(data, ...)
        
        ## Cache the inverted matrix for future use
        x$setinverse(i)
        i        
}
