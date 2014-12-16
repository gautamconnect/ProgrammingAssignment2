## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setInv <- function(inv) {
                i <<- inv
        }
        
        getInv <- function() {
                i
        }
        
        list(set = set, get = get,
             setInv = setInv, 
             getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInv()
        
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        
        m <- x$get()
        i <- solve(m)
        x$setInv(i)
        i        
}
