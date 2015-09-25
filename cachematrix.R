
# makeCacheMatrix returns a list of functions - set, get, setInverse abd getInverse. 
# cacheSolve returns the inverse of a square matrix
# The two functions can be used together by first using makeCacheMatrix() as an anonymous function stored in a variable, and then using the set() function to set a square matrix that you want to invert.
# For e.g., you can set c <- makeCacheMatrix(); c$set(matrix(c(7,8,10,12),nrow=2, ncol=2));
# Then, when you run cacheSolve(c), you will get the inverse of the matrix created using the set command.
# In the event that the matrix had already been inverted using the setInverse function contained in makeCacheMatrix, R will get the cached inverse instead of re-calculating, and it will print out "getting cached inverse of matrix"
# The whole function could be written in one line, like this:
# cacheSolve(makeCacheMatrix(matrix(c(7,8,10,12), nrow=2, ncol=2)))
# It should be noted that if you run the function all in one line, then it doesn't actually get the cached version if one exists. I'm not sure why but I'm assuming it has something to do with R's lexical scoping.



# MakeCacheMatrix is to be used as anonymous function that will be stored in a variable. 
# This variable, if set by using the set function contained within this makeCacheMatrix function, will be passed through the cacheSolve function.
# When it is passed through the cacheSolve function, that function will use a cached version of inverted matrix (through the getInverse variable)

makeCacheMatrix <- function(x = matrix()) {


        m <- NULL #sets m as NULL
        set <- function(y) { 
                x <<- y
                m <<- NULL
        }
        
#this function returns the matrix
get <- function() x

#this function finds the inverse of the matrix and caches it
setInverse <- function(solve) m <<- solve

#this function returns the stored/cached matrix
getInverse <- function() m

#returns a list of the functions used in the makeCacheMatrix function
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# cacheSolve is a function that takes as an argument a square matrix and returns the inverse. 
# The inverse will either be calculated by R or it will use a cached version of the inverse if it is already stored in R's memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        # If cached inverse of the matrix exists, it will display a message telling me this.
        if(!is.null(m)) {
                message("getting cached inverse of matrix")
                return(m)
        }
        
        # If cached inverse doesn't exist, R will find the inverse of the square matrix using the 'solve' function.
        matrix1 <- x$get()
        m <- solve(matrix1, ...)
        x$setInverse(m)
        
        m
                
        
}
