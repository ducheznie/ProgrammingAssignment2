## Matrix inversion is usually a costly computation and there may
## be some benefits to caching the inverse of a matrix rather than
## compute it repeatedly

## makeCacheMatrix creates a special "matrix" object, which is a list
## containing a funciton to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinv(inv)
        
        inv
}
