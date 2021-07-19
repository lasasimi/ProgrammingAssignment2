## 'makeCacheMatrix' will store a cache by creating a special "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        ## Asign inv in parent environment
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        ## List to refer the functions with $
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The function computes the inverse of the matrix returned by the function above
## 'cacheSolve' should retrieve from cache if the inverse has been calculated

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
