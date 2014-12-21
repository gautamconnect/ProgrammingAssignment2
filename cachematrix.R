## The program will cache the results of inverting a matrix
## When returning the result, first a cache will be checked 
## If the cache has the result, then results are fetched from cache
## Else, results are calculated. 

## The function makeCacheMatrix creates a "matrix", which is actually a list 
## containing functions which are used to get/set the value of the matrix
## and to get/set the value of inverse of the matrix. 

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


## The following function calculates the inverse of "matrix" created using the 
## function above. cacheSolve checks if the inverse already exists. If so, the
## results are fetched from cache and returned. Else, inverse is calulated. 

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
