## Put comments here that give an overall description of what your
## functions do

# This function is to be used as anonymous function that will be stored in a variable. 
# This variable, if set by using the set function contained within this makeCacheMatrix function, will be passed through the cacheSolve function.
# When it is passed through the cacheSolve function, that function will use a cached version of inverted matrix (through the getInverse variable)

makeCacheMatrix <- function(x = matrix()) {


        m <- NULL #sets m as NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        
        
get <- function() x


setInverse <- function(solve) m <<- solve
getInverse <- function() m

list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function that takes as an argument a matrix and returns the inverse. 
#The inverse will either be calculated by R or it will use a cached version of the inverse if it is stored in R's memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached inverse of matrix")
                return(m)
        }
        matrix1 <- x$get()
        m <- solve(matrix1, ...)
        x$setInverse(m)
        
        m
                
        
}
